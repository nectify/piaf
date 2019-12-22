include Lwt

module Syntax = struct
  let ( let+ ) x f = map f x

  let ( let* ) = bind
end

module Result = struct
  include Lwt_result

  module Syntax = struct
    let ( let+ ) x f = map f x

    let ( let* ) = bind
  end
end

module Stream = Lwt_stream

(* HACK: remove to another module *)
let resolve_host ~port hostname =
  let open Syntax in
  let+ addresses =
    Lwt_unix.getaddrinfo
      hostname
      (string_of_int port)
      (* https://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml *)
      Unix.[ AI_CANONNAME; AI_PROTOCOL 6; AI_FAMILY PF_INET ]
  in
  match addresses with
  | [] ->
    let msg = Format.asprintf "Can't resolve hostname: %s" hostname in
    Error msg
  | xs ->
    (* TODO: add resolved canonical hostname *)
    Ok (List.map (fun { Unix.ai_addr; _ } -> ai_addr) xs)

module Unix = struct
  open Lwt_unix

  type fd = file_descr

  let with_timeout = with_timeout

  let connect = connect

  let close = close

  let socket = socket

  let setsockopt = setsockopt
end

include Unix
