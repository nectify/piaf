open H2

module type Client = sig
  type +'a io

  type t

  type socket

  val create_connection
    :  ?config:Config.t
    -> ?push_handler:
         (Request.t -> (Client_connection.response_handler, unit) result)
    -> error_handler:Client_connection.error_handler
    -> socket
    -> t io

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
    -> (t, string) result io

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
