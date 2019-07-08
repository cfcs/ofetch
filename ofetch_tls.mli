type 'underlying_t tls_t = {
  state : Tls.Engine.state;
  wire_send_queue : string list;
  plaintext_incoming : Cstruct.t option ;
  underlying_t : 'underlying_t;
}
type 'a tt = Passthru of 'a | TLS of 'a tls_t
module Wrap_tls :
  functor (Underlying : Ofetch.Peer_S) ->
    ( Ofetch.Peer_S
      with
        type t = Underlying.t tt
       and type init_data = Underlying.init_data
    )
