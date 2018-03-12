type init_data = (Unix.inet_addr * int)

type t = { peer_fd : Unix.file_descr ;
           buflen : int ;
         }

let equal a b = a = b

let rec retry_signals f =
  let open Unix in
  match f () with
  | exception Unix_error ((EAGAIN | EINTR), _ , _ ) ->
    retry_signals f
  | exception Unix_error (ECONNRESET, _, _ ) -> failwith "econnresetyo"
  | exception Unix_error (ECONNREFUSED, _, _ ) -> failwith "econnrefused"
  | ok -> ok

let select
    (read_handles  : t list )
    (write_handles : t list )
    (except_handles: t list )
  : t list * t list * t list =
  (* TODO raises EINVAL when a closed socket is used.
      - Why the fuck doesn't it report *which* fd?
     TODO if -1 == uerror("select", Nothing);
     TODO how to handle timeout?
  *)
  let read = List.map (fun r -> r.peer_fd) read_handles in
  let write = List.map (fun r -> r.peer_fd) write_handles in
  let except = List.map (fun r -> r.peer_fd) except_handles in
  match retry_signals
          (fun () -> UnixLabels.select ~read ~write ~except ~timeout:45.0)
  with
  | r, w, e ->
    let get_handle needle haystack =
      List.find (fun {peer_fd ; _ } -> peer_fd = needle) haystack in
    let handles_of_peer_fds fds haystack =
      List.map (fun fd -> get_handle fd haystack) fds in
    (handles_of_peer_fds r read_handles),
    (handles_of_peer_fds w write_handles),
    (handles_of_peer_fds e except_handles)

let recv_peer t ~buf ~pos ~len =
  UnixLabels.read t.peer_fd ~buf ~pos ~len, t
let write_peer t ~buf ~pos ~len =
  Unix.write_substring t.peer_fd buf pos len, t

let init ~protocol ((inet_addr, port) : init_data) =
  if protocol <> "http"
  then Error "unix http-only transport does not support your protocol" (*TODO*)
  else
  (* not using UnixLabels here since 4.05 introduces new args: *)
  let peer_fd = Unix.(socket PF_INET SOCK_STREAM 0) in
  let open UnixLabels in
  set_nonblock peer_fd;
  (* TODO set SO_SIGNOPIPE [BSD only?], or use MSG_NOSIGNAL
     when sending or similar? *)
  (* TODO set SO_LINGER ? *)
  (* TODO set SO_KEEPALIVE ? *)
  (*let buflen = UnixLabels.() in*)
  let addr = ADDR_INET (inet_addr, port) in
  match retry_signals (fun () -> connect peer_fd ~addr) with
  | exception Unix_error (EINPROGRESS, _, _ ) ->
    Ok { peer_fd ; buflen = getsockopt_int peer_fd SO_RCVBUF ; }
  | exception  _ -> Error "error connecting tcp socket"
  | () -> Ok { peer_fd ; buflen = getsockopt_int peer_fd SO_RCVBUF ; }

(* TODO
let unix_finalize fd : unit = Unix.close fd
*)
