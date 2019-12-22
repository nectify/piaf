module type HTTPCommon = Http_intf.HTTP with type +'a Client.io = 'a Lwt.t

module type HTTP = HTTPCommon with type Client.socket = Lwt_unix.file_descr

module type HTTPS = HTTPCommon with type Client.socket = Tls.socket

module type HTTP2 =
  Http_intf.HTTP2
    with type +'a Client.io = 'a Lwt.t
     and type Client.socket = Lwt_unix.file_descr
