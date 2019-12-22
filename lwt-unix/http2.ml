(*----------------------------------------------------------------------------
 * Copyright (c) 2019, António Nuno Monteiro
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *---------------------------------------------------------------------------*)

module type Client = sig
  open H2

  type t

  type socket

  val create_connection
    :  ?config:Config.t
    -> ?push_handler:
         (Request.t -> (Client_connection.response_handler, unit) result)
    -> error_handler:Client_connection.error_handler
    -> socket
    -> t Io.t

  (* From RFC7540§3.1:
   *   The string "h2c" identifies the protocol where HTTP/2 is run over
   *   cleartext TCP. This identifier is used in the HTTP/1.1 Upgrade header
   *   field and in any place where HTTP/2 over TCP is identified. *)
  val create_h2c_connection
    :  ?config:Config.t
    -> ?push_handler:
         (Request.t -> (Client_connection.response_handler, unit) result)
    -> http_request:Httpaf.Request.t
    -> error_handler:Client_connection.error_handler
    -> Client_connection.response_handler * Client_connection.error_handler
    -> socket
    -> (t, string) result Io.t

  val request
    :  t
    -> Request.t
    -> error_handler:Client_connection.error_handler
    -> response_handler:Client_connection.response_handler
    -> [ `write ] Body.t

  val ping : t -> ?payload:Bigstringaf.t -> ?off:int -> (unit -> unit) -> unit

  val shutdown : t -> unit

  val is_closed : t -> bool
end

let make_error_handler real_handler type_ error =
  let error : Http_intf.error =
    match error with
    | `Invalid_response_body_length response ->
      `Invalid_response_body_length (Response.of_h2 response)
    | (`Exn _ | `Malformed_response _ | `Protocol_error _) as other ->
      other
  in
  real_handler (type_, error)

module MakeHTTP2 (H2_client : Client) :
  Http_intfs.HTTPCommon
    with type Client.t = H2_client.t
     and type Client.socket = H2_client.socket
     and type Body.Read.t = [ `read ] H2.Body.t
     and type Body.Write.t = [ `write ] H2.Body.t = struct
  module Body :
    Http_intf.Body
      with type Read.t = [ `read ] H2.Body.t
       and type Write.t = [ `write ] H2.Body.t = struct
    module Read = struct
      type t = [ `read ] H2.Body.t

      include (
        H2.Body : module type of H2.Body with type 'rw t := 'rw H2.Body.t)
    end

    module Write = struct
      type t = [ `write ] H2.Body.t

      include (
        H2.Body : module type of H2.Body with type 'rw t := 'rw H2.Body.t)
    end
  end

  module Client = struct
    include H2_client

    (* TODO: remove via module substitution *)
    type +'a io = 'a Lwt.t

    let create_connection ?config:_ ~error_handler fd =
      create_connection
        ~error_handler:(make_error_handler error_handler `Connection)
        fd

    type response_handler = Response.t -> Body.Read.t -> unit

    let request t req ~error_handler ~response_handler =
      let response_handler response body =
        response_handler (Response.of_h2 response) body
      in
      request
        t
        (Request.to_h2 req)
        ~error_handler:(make_error_handler error_handler `Stream)
        ~response_handler
  end
end

module Make_HTTP2c (H2_client : Client with type socket = Lwt_unix.file_descr) :
  Http_intfs.HTTP2 = struct
  module HTTP_X :
    Http_intfs.HTTPCommon
      with type Client.t = H2_client.t
       and type Client.socket = H2_client.socket
       and type Body.Read.t = [ `read ] H2.Body.t
       and type Body.Write.t = [ `write ] H2.Body.t =
    MakeHTTP2 (H2_client)

  include (HTTP_X : module type of HTTP_X with module Client := HTTP_X.Client)

  module Client = struct
    include HTTP_X.Client

    let create_h2c_connection
        ?config:_
        ?push_handler:_
        ~http_request
        ~error_handler
        (response_handler, response_error_handler)
        fd
      =
      let response_handler response body =
        response_handler (Response.of_h2 response) body
      in
      let response_error_handler error =
        let error : Http_intf.error =
          match error with
          | `Invalid_response_body_length response ->
            `Invalid_response_body_length (Response.of_h2 response)
          | (`Exn _ | `Malformed_response _ | `Protocol_error _) as other ->
            other
        in
        response_error_handler (`Stream, error)
      in
      H2_client.create_h2c_connection
        ~http_request
        ~error_handler:(make_error_handler error_handler `Connection)
        (response_handler, response_error_handler)
        fd
  end
end

module HTTP : Http_intfs.HTTP2 = Make_HTTP2c (H2_lwt_unix.Client)

module HTTPS : Http_intfs.HTTPS = MakeHTTP2 (H2_lwt_unix.Client.SSL)
