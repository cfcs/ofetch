(*module M :
  Ofetch.Peer_S with type init_data = Ofetch_unix.init_data
  = Ofetch_tls.Wrap_tls(Ofetch_unix)
include M
*)

let () = Nocrypto_entropy_unix.initialize ()

include Ofetch_tls.Wrap_tls(Ofetch_unix)
