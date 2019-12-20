open Piaf

module HTTP : Http_intf.HTTP = MakeHTTP1 (struct
  type +'a io = 'a Lwt.t

  include Httpaf_lwt_unix.Client
end)

module HTTPS : Http_intf.HTTPS = MakeHTTP1 (struct
  type +'a io = 'a Lwt.t

  include Httpaf_lwt_unix.Client.SSL
end)
