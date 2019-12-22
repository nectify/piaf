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

module Io : sig
  type +'a t

  type -'a u

  val wait : unit -> 'a t * 'a u

  val wakeup_later : 'a u -> 'a -> unit

  val wakeup : 'a u -> 'a -> unit

  val return : 'a -> 'a t

  val return_unit : unit t

  val async : (unit -> unit t) -> unit

  val pick : 'a t list -> 'a t

  val choose : 'a t list -> 'a t

  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t

  module Syntax : sig
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module Result : sig
    type +'a io := 'a t

    type (+'a, +'b) t = ('a, 'b) result io

    val return : 'a -> ('a, _) t

    val fail : 'b -> (_, 'b) t

    val lift : ('a, 'b) result -> ('a, 'b) t

    val ok : 'a io -> ('a, _) t

    val map : ('a -> 'b) -> ('a, 'e) t -> ('b, 'e) t

    val map_err : ('e1 -> 'e2) -> ('a, 'e1) t -> ('a, 'e2) t

    val bind : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t

    module Infix : sig
      val ( >|= ) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t

      val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
    end

    include module type of Infix

    module Syntax : sig
      val ( let+ ) : ('a, 'b) t -> ('a -> 'c) -> ('c, 'b) t

      val ( let* ) : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
    end
  end

  module Stream : sig
    type +'a io := 'a t

    type 'a t

    val from : (unit -> 'a option io) -> 'a t

    val of_list : 'a list -> 'a t

    val map : ('a -> 'b) -> 'a t -> 'b t

    val iter : ('a -> unit) -> 'a t -> unit io

    val junk_while : ('a -> bool) -> 'a t -> unit io

    val junk_old : 'a t -> unit io

    val closed : 'a t -> unit io
  end

  (* HACK: remove to another module *)
  val resolve_host : port:int -> string -> (Unix.sockaddr list, string) result t

  type fd

  val with_timeout : float -> (unit -> 'a t) -> 'a t

  val connect : fd -> Unix.sockaddr -> unit t

  val close : fd -> unit t

  val socket : Unix.socket_domain -> Unix.socket_type -> int -> fd

  val setsockopt : fd -> Unix.socket_bool_option -> bool -> unit
end

module TLS : sig
  type socket = Lwt_ssl.socket

  val connect
    :  hostname:string
    -> config:Config.t
    -> alpn_protocols:string list
    -> Io.fd
    -> (socket, string) result Io.t
end

module type HTTPCommon = Http_intf.HTTP with type +'a Client.io = 'a Io.t

module type HTTP = HTTPCommon with type Client.socket = Io.fd

module type HTTPS = HTTPCommon with type Client.socket = TLS.socket

module type HTTP2 =
  Http_intf.HTTP2
    with type +'a Client.io = 'a Io.t
     and type Client.socket = Io.fd

module HTTP1 : sig
  module HTTP : HTTP

  module HTTPS : HTTPS
end

module HTTP2 : sig
  module HTTP : HTTP2

  module HTTPS : HTTPS
end
