type 'underlying_t tls_t =
  { state : Tls.Engine.state ;
    wire_send_queue : bytes list ;
    underlying_t : 'underlying_t ;
  }

type 'a tt =
  | HTTP of 'a
  | HTTPS of 'a tls_t

module Wrap_tls : functor (Underlying : Ofetch.Peer_S) ->
  Ofetch.Peer_S with type t = Underlying.t tt
                 and type init_data = Underlying.init_data
  =
  functor (Underlying : Ofetch.Peer_S) ->
  struct
    type t = Underlying.t tt
    type init_data = Underlying.init_data
    let equal a b =
      match a,b with
      | HTTP a, HTTP b -> Underlying.equal a b
      | HTTPS a, HTTPS b ->
        Underlying.equal a.underlying_t b.underlying_t
      | _ -> false

    let pop_queued t =
      match t.wire_send_queue with
      | [] -> t
      | hd::tl ->
        let len = Bytes.length hd in
        begin match Underlying.write_peer t.underlying_t
                      ~buf:(Bytes.to_string hd) ~pos:0 ~len
          with
          | 0 , _ -> failwith "fuck"
          | n , _ when n = len -> {t with wire_send_queue = tl; }
          | n , _ ->
            {t with wire_send_queue =
                      Bytes.(sub hd n (len-n))::tl;}
        end

    let select_tls_t (readable : 'fd list) (writeable : 'fd list)
        (except : 'fd list)
      : 'fd list * 'fd list * 'fd list =
      let underlying_t = List.map (fun t -> t.underlying_t) in
      let our_t selected_lst original =
        List.map (fun selected ->
            List.find (fun o -> o.underlying_t = selected) original)
          selected_lst
      in
      (* first we determine which TLS sessions are ready to receive data: *)
      (*let readable, _ = List.partition
          (fun t -> Tls.Engine.can_handle_appdata t.state) readable in*)
      let writeable, _ = List.partition
          (fun t -> Tls.Engine.can_handle_appdata t.state
                    ||  [] <> t.wire_send_queue) writeable in

      (* Then we determine which TLS sessions have selectable underlying fds: *)
      match Underlying.select
              (underlying_t readable)
              (underlying_t writeable)
              (underlying_t except) with
      | r, w, e ->
        let w_app, w_queued =
          List.partition (fun t -> t.wire_send_queue = [] ) (our_t w writeable)
        in
        let w_queued = List.map pop_queued w_queued in
        (* TODO how to return changed states here? *)
        (our_t r readable), w_app, (our_t e except)

    let select readable writeable except =
      let r_http, r_https =
        List.partition (function HTTP _ -> true | _ -> false) readable in
      let w_http, w_https =
        List.partition (function HTTP _ -> true | _ -> false) writeable in
      let e_http, e_https =
        List.partition (function HTTP _ -> true | _ -> false) except in
      let to_u = List.map (function HTTP fd -> fd | HTTPS _ ->failwith "OOPS")in
      let of_u = List.map (function fd -> HTTP fd) in
      let hr, hw, he =
        let a, b, c =
          Underlying.select (to_u r_http) (to_u w_http) (to_u e_http)
        in (of_u a), (of_u b), (of_u c)
      in
      let to_s = List.map (function
          | HTTPS fd -> fd
          | HTTP _ -> failwith "TODO HTTP not expected here") in
      let of_u = List.map (function fd -> HTTPS fd) in
      let sr, sw, se = select_tls_t (to_s r_https) (to_s w_https) (to_s e_https)
      in (of_u sr)@hr, (of_u sw)@hw, (of_u se)@he (* try to consolidate *)

    let init ~protocol (underlying_init : Underlying.init_data) =
      begin match protocol with
        | "http" -> begin match Underlying.init ~protocol underlying_init with
            | Ok x -> Ok (HTTP x)
            | Error x -> Error x
          end
        | "https" ->
          let tls_config : Tls.Config.client =
            (* cacerts.pem, fp pinning, .. *)
            Tls.Config.client ~authenticator:X509.Authenticator.null ()
          in
          let state, to_send = Tls.Engine.client tls_config in
          begin match Underlying.init ~protocol:"http" underlying_init with
          | Ok underlying_t ->
            Ok (HTTPS {state ;
                       underlying_t ;
                       wire_send_queue = [Cstruct.to_bytes to_send]})
          | Error x -> Error x
          end
        | _ -> Error "unsupport http(s) protocol"
      end

    let recv_peer_tls_t fd ~buf ~(pos:int) ~(len:int) : int * t =
      (* Initialize tmp buf: *)
      let ubuf = Bytes.init len (fun _ -> '\x00') in
      let len_ret, underlying_t =
        Underlying.recv_peer fd.underlying_t ~buf ~pos ~len
      in
      let received = Cstruct.of_bytes (Bytes.sub ubuf 0 len_ret) in
      begin match Tls.Engine.handle_tls fd.state received with
        | `Ok ((`Eof | `Alert _), _, `Data (Some data)) ->
          (* ignore response, kill connection, return data *)
          (* TODO invalidate state *)
          0, HTTPS {state = fd.state; underlying_t ; wire_send_queue = [] }
        | `Ok ((`Eof | `Alert _), _, `Data None)
        | `Fail (_, _) ->
          (* ignore response, kill connection *)
          (* TODO invalidate state *)
          ignore @@ failwith "TODO invalidate state on tls error" ;
          0, HTTPS {state = fd.state; underlying_t ; wire_send_queue = [] }
        | `Ok (`Ok state, `Response resp, `Data recvd) ->
          (* set new [state], send [resp], recv [data] *)
          begin match recvd with
            | None -> 0
            | Some recvd_data -> Cstruct.len recvd_data
          end,
          begin match resp with
            | Some resp_data ->
              HTTPS { state; underlying_t ;
                wire_send_queue =
                  (Cstruct.to_bytes resp_data)::fd.wire_send_queue
              }
            | None -> HTTPS {fd with state; underlying_t }
          end
      end

    let recv_peer tt ~buf ~pos ~len =
      match tt with
      | HTTPS fd -> recv_peer_tls_t fd ~buf ~pos ~len
      | HTTP fd -> begin match Underlying.recv_peer fd ~buf ~pos ~len with
          | rlen, nfd -> rlen, HTTP nfd
          end

    let write_peer_tls_t (t) ~buf ~pos ~len =
      let sendlist =
        Cstruct.of_string (if pos = 0 && len = String.length buf
                           then buf
                           else String.sub buf pos len)
      in
      begin match Tls.Engine.send_application_data t.state [sendlist] with
        | None -> failwith "TODO trying to send to invalid state"
        | Some (state, to_send) ->
          len,
          { t with
            state ;
            wire_send_queue = (Cstruct.to_bytes to_send)::t.wire_send_queue; }
      end

    let write_peer t ~buf ~pos ~len =
      match t with
      | HTTP fd -> begin match Underlying.write_peer fd ~buf ~pos ~len with
          | rlen, nfd -> rlen, HTTP nfd
        end
      | HTTPS fd ->
        let wlen, nfd = write_peer_tls_t fd ~buf ~pos ~len in
        wlen, HTTPS nfd
end
