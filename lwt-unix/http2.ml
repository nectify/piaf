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

module HTTP : Http_intf.HTTP2 = struct
  module HTTP_X :
    Http_intf.HTTPCommon
      with type Client.t = H2_lwt_unix.Client.t
       and type Client.socket = Lwt_unix.file_descr
      with type Body.Read.t = [ `read ] H2.Body.t
       and type Body.Write.t = [ `write ] H2.Body.t
       and type +'a Client.io = 'a Lwt.t = MakeHTTP2 (struct
    type +'a io = 'a Lwt.t

    include H2_lwt_unix.Client
  end)

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
      H2_lwt_unix.Client.create_h2c_connection
        ~http_request
        ~error_handler:(make_error_handler error_handler `Connection)
        (response_handler, response_error_handler)
        fd
  end
end

module HTTPS : Http_intf.HTTPS = MakeHTTP2 (struct
  type +'a io = 'a Lwt.t

  include H2_lwt_unix.Client.SSL
end)
