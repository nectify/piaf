open Httpaf

module type Client = sig
  type +'a io

  type t

  type socket

  val create_connection : ?config:Config.t -> socket -> t io

  val request
    :  t
    -> Request.t
    -> error_handler:Client_connection.error_handler
    -> response_handler:Client_connection.response_handler
    -> [ `write ] Body.t

  val shutdown : t -> unit

  val is_closed : t -> bool
end
