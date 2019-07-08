type 'underlying_t tls_t =
  { state : Tls.Engine.state ;
    wire_send_queue : string list ;
    plaintext_incoming : Cstruct.t option ;
    underlying_t : 'underlying_t ;
  }

type 'a tt =
  | Passthru of 'a
  | TLS of 'a tls_t

open Ofetch.Results

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
      | Passthru a, Passthru b ->
        Underlying.equal a b
      | TLS a, TLS b ->
        Underlying.equal a.underlying_t b.underlying_t
      | TLS a, Passthru b ->
        Underlying.equal a.underlying_t b
      | Passthru a, TLS b ->
        Underlying.equal a b.underlying_t


    let pop_queued t =
      match t.wire_send_queue with
      | [] -> false, t
      | hd::tl ->
        true,
        let len = String.length hd in
        Ofetch.debug (fun m -> m"TLS: GOING TO WRITE %d\n%!" len) ;
        begin match Underlying.write_peer t.underlying_t
                      ~buf:hd ~pos:0 ~len
          with
          | 0 , underlying_t ->   { t with underlying_t }
          | n , underlying_t when n = len ->
            { t with underlying_t ; wire_send_queue = tl; }
          | n , underlying_t ->
            {t with underlying_t ;
                    wire_send_queue =
                      ( if len-n > 0 then [String.(sub hd n (len-n))]
                        else begin assert (0=len-n); [] end) @ tl;}
        end

    let tls_is_writeable t =
      Tls.Engine.can_handle_appdata t.state (* next write will be app data*)

    let tls_needs_write_from_queue t =
      [] <> t.wire_send_queue (* next write will send from queue *)

(*
    let select_tls_t (readable : 'fd list) (writeable : 'fd list)
        (except : 'fd list)
      : 'fd list * 'fd list * 'fd list =
      let our_t selected_lst original =
        List.map (fun selected ->
            List.find (fun o -> o.underlying_t = selected) original)
          selected_lst
      in
      (* first we determine which TLS sessions are ready to receive data: *)
      (*let readable, _ = List.partition
          (fun t -> Tls.Engine.can_handle_appdata t.state) readable in*)
      let writeable, _ = List.partition tls_is_writeable writeable in
      readable, writeable, except
                           *)
    let select (readable:Underlying.t tt list) (writeable:Underlying.t tt list)
        (except:Underlying.t tt list)
      : Underlying.t tt list * Underlying.t tt list * Underlying.t tt list =
      let to_u = List.rev_map (function
          | Passthru fd -> fd
          | TLS t -> t.underlying_t) in

      Ofetch.debug (fun m -> m"TLS: SELECT %d %d %d\n"
                       (List.length readable) (List.length writeable)
                       (List.length except) );
      let u_w, _ = List.partition (function
          | Passthru _ -> true | TLS t ->
            let x = tls_is_writeable t and y = tls_needs_write_from_queue t in
            Ofetch.debug (fun m ->
                m"TLS: is writeable: %b needs writes from queue: %b\n%!" x y);
            x || y) writeable in
      let u_r, u_w, u_e =
        Underlying.select (to_u readable) (to_u u_w) (to_u except)
      in
      (let l = List.length in
       Ofetch.debug (fun m -> m"HTTP(S?): %d %d %d\n"
                        (l readable) (l writeable) (l except)));
      List.rev_map (fun ut -> List.find (function
          | Passthru xxx -> Underlying.equal ut xxx
          | TLS xxx -> Underlying.equal ut xxx.underlying_t) readable) u_r,
      List.rev_map (fun ut -> List.find (function
          | Passthru xxx -> Underlying.equal ut xxx
          | TLS xxx -> Underlying.equal ut xxx.underlying_t) writeable) u_w,
      List.rev_map (fun ut -> List.find (function
          | Passthru xxx -> Underlying.equal ut xxx
          | TLS xxx -> Underlying.equal ut xxx.underlying_t) except) u_e

    let init ~(protocol)
        (underlying_init : init_data) : (t, string) result =
      begin match protocol with
        | "tls" ->
          let tls_config : Tls.Config.client =
            (* cacerts.pem, fp pinning, .. *)
            Tls.Config.client ~authenticator:X509.Authenticator.null ()
          in
          let state, to_send = Tls.Engine.client tls_config in
          Underlying.init ~protocol:"tcp" underlying_init
            >>| fun underlying_t ->
            (TLS {state ; underlying_t ; plaintext_incoming = None ;
                    wire_send_queue = [Cstruct.to_string to_send]})
        | "tcp" ->
          Underlying.init ~protocol:"tcp" underlying_init >>| fun x -> Passthru x
        | protocol ->
          Error ("TLS: Unsupported protocol: " ^ String.escaped protocol )
      end

    let recv_peer_tls_t (fd: 'a tls_t) ~buf ~(pos:int) ~(len:int)
      : (int * t, string) result =
      assert (fd.plaintext_incoming = None);
      Ofetch.debug (fun m -> m"TLS: reading from underlying (pos:%d)\n%!" pos);
      let underlying_buf = Bytes.init len (fun _ -> '\x00') in
      Underlying.recv_peer fd.underlying_t ~buf:underlying_buf ~pos:0 ~len
      >>= fun (len_ret, underlying_t) ->
      Ofetch.debug (fun m -> m"TLS: received: %S\n%!"
                       (Bytes.sub_string underlying_buf 0 len_ret));
      begin match Tls.Engine.handle_tls fd.state @@
        let cs = Cstruct.create len_ret in
        Cstruct.blit_from_bytes underlying_buf 0 cs 0 len_ret; cs with
      | `Ok ((`Eof | `Alert _), `Response _, `Data (Some data)) ->
        (* ignore response, kill connection, return data.
           TODO maybe we should try to handle the Response. *)
        Error (Cstruct.to_string data)
      | `Ok ((`Eof | `Alert _), _, `Data None) ->
        Ofetch.debug (fun m -> m"TLS: EOF or ALERT with NO data\n%!");
        Error ""
      | `Fail (msg, _) ->
        (* ignore response, kill connection *)
        Ofetch.debug (fun m ->
            m"TLS: FAIL: %s\n%!" (Tls.Engine.string_of_failure msg));
        Error ""
      | `Ok (`Ok state, `Response resp, `Data recvd) ->
        (* set new [state], send [resp], recv [data] *)
        let ret_bytes, plaintext_incoming =
          begin match recvd with
            | None ->
              Ofetch.debug (fun m -> m"TLS: recvd len 0\n%!");
              0, None
            | Some recvd_data ->
              let recvd_len = Cstruct.len recvd_data in
              let actual_len = max 0 (min recvd_len (len-pos)) in
              Cstruct.blit_to_bytes recvd_data 0 buf pos actual_len ;
              Ofetch.debug (fun m -> m"TLS: rcvd (err) from upstream: %d: %S\n%!"
                               recvd_len (Bytes.sub_string buf pos actual_len));
              actual_len, if actual_len <> recvd_len then
                Some (Cstruct.shift recvd_data actual_len) else None
          end
        in
        Ok (ret_bytes,
            TLS { state; underlying_t ; plaintext_incoming ;
                  wire_send_queue = fd.wire_send_queue @ match resp with
                    | Some resp_data when Cstruct.len resp_data <> 0 ->
                      [Cstruct.to_string resp_data]
                    | Some _ | None -> []
                })
      end

    let recv_peer (tt:t) ~(buf:bytes) ~pos ~len
      : (int * Underlying.t tt,string) result=
      match tt with
      | TLS ({ plaintext_incoming = None; _ } as fd) ->
        Ofetch.debug (fun m -> m"TLS: Ofetch.recv_peer getting TLS stream\n%!");
        recv_peer_tls_t fd ~buf ~pos ~len
      | TLS ({ plaintext_incoming = Some plaintext ; _ } as fd) ->
        let rlen = max 0 (min (len-pos) (Cstruct.len plaintext)) in
                Ofetch.debug (fun m ->
            m"TLS: Ofetch.recv_peer returning buffered data (%d/%d)\n%!"
              rlen (Cstruct.len plaintext - rlen));
        Cstruct.blit_to_bytes plaintext 0 buf pos rlen ;
        let plaintext_incoming =
          if rlen < Cstruct.len plaintext then begin
            Some (Cstruct.shift plaintext rlen) end else None in
        Ok (rlen, TLS {fd with plaintext_incoming})
      | Passthru fd ->
        Ofetch.debug (fun m ->
            m"TLS: Ofetch.recv_peer getting some Passthru data\n%!");
        begin match Underlying.recv_peer fd ~buf ~pos ~len with
          | Ok (rlen, nfd) -> Ok (rlen, Passthru nfd)
          | Error _ as err -> err
        end

    let write_peer_tls_t (t) ~buf ~pos ~len =
      let no_more, t = pop_queued t in
      if no_more || len = 0 then
        0, t
      else begin
        match
          let plain = Cstruct.of_string ~off:pos ~len buf in
          Tls.Engine.send_application_data t.state [plain] with
        | None -> failwith "TODO trying to send to invalid state"
        | Some (state, to_send) ->
          let to_send = Cstruct.to_string to_send in
          let written_len, underlying_t =
            Underlying.write_peer t.underlying_t ~pos:0 ~buf:to_send
              ~len:(String.length to_send)
          in
          len,
          { state; underlying_t;
            plaintext_incoming = t.plaintext_incoming;
            wire_send_queue =
              (if written_len <> String.length to_send then
                 [String.sub to_send (pos+written_len) (len-written_len)
                 ] else [])
              @ t.wire_send_queue
              @ [String.sub buf pos len]
          }
      end

    let write_peer t ~buf ~pos ~len =
      match t with
      | Passthru fd -> begin match Underlying.write_peer fd ~buf ~pos ~len with
          | rlen, nfd -> rlen, Passthru nfd
        end
      | TLS fd ->
        Ofetch.debug (fun m -> m"Ofetch_tls.write_peer writing %S\n%!"
                         (String.sub buf pos len));
        let wlen, nfd = write_peer_tls_t fd ~buf ~pos ~len in
        wlen, TLS nfd
end
