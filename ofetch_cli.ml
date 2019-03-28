(* Enable cancellation of all downloads to fs that is out of space *)
type fs_id = int * int
let fs_id_of_fd fd : fs_id =
  let open UnixLabels in
  let stat = fstat fd in
  stat.st_dev , stat.st_rdev

type unix_handle =
  { fs: fs_id ;
    download_file : Unix.file_descr ;
    peer_fd : Ofetch_wrap.t ref ;
  }

let is_same_fs fs_id handle = (* helper for failing downloads to full disk *)
  fs_id = handle.fs

let try_chroot () =
  let open Unix in
  try chroot "." ; chdir "/"
  with Unix_error (EPERM, _, _ ) -> ()

let fetch_conn protocol inet_addr port =
  try_chroot () ;
  Ofetch_wrap.init ~protocol (inet_addr, port)

let open_new_file ~filename =
  if filename = "-" then Ok Unix.stdout else
  try Ok (Unix.openfile filename Unix.[O_CREAT ; O_WRONLY; O_EXCL] 0o600)
  with
  | Unix.(Unix_error (a, b, c)) -> Error (a,b,c)

open Ofetch.Results

let rec retry_signals f =
  let open Unix in
  match f () with
  | exception Unix_error ((EAGAIN | EINTR), _ , _ ) -> retry_signals f
  | exception Unix_error (ECONNRESET, _, _ ) -> failwith "econnresetyo"
  | exception Unix_error (ECONNREFUSED, _, _ ) -> failwith "econnrefused"
  | ok -> ok

let mkconn ~protocol ~(addr:Unix.inet_addr) ~hostname ~port ~uri ~local_filename
  : (unix_handle Ofetch.request, string) result =
  let peer_fd =
    match fetch_conn protocol addr port with
    | Ok x -> ref x
    | Error s -> failwith ("failed to connect:" ^ s)
  in
  begin match open_new_file ~filename:local_filename with
    | Ok fd -> Ok fd
    | Error (Unix.EEXIST, "open", fn) ->
      Error (Format.asprintf "Output file %S already exists." fn)
    | Error _ -> Error "unix error while opening file"
  end >>| fun file_channel ->
  (*let mmap fd =
    (*let open Unix.LargeFile in*)
    let open Bigarray in
    (*fstat  http://caml.inria.fr/pub/docs/manual-ocaml/libref/UnixLabels.LargeFile.html *)
    let seek = 0L and shared = true and dimensions = (-1) in
    (Bigarray.Array1.map_file [@ocaml.warnerror "-3"]) fd ~pos:seek Char C_layout shared dimensions
  in*)
  let buflen = 8192 in (*TODO detect from Peer_S *)
  let write_local buf pos len =
    if len = 0 then 0 else (* fast track *)
      retry_signals (fun () -> UnixLabels.write file_channel ~buf ~pos ~len) in
  let recv_peer buf pos len =
    retry_signals (fun () -> (* TODO fd stays the same, so ignore it: *)
        match Ofetch_wrap.recv_peer !peer_fd ~buf ~pos ~len with
        | Ok (new_len, new_fd) -> peer_fd := new_fd; Ok new_len
        | Error _ as err -> err
      ) in
  let write_peer buf pos len =
    retry_signals (fun () ->
        match Ofetch_wrap.write_peer !peer_fd ~buf ~pos ~len with
        new_len, new_fd -> peer_fd := new_fd ; new_len
      ) in
  let handle =
    { fs = fs_id_of_fd file_channel ;
      download_file = file_channel ;
      peer_fd ;
    }
  in
  Ofetch.new_request ~connection_handle:handle
    ~buflen ~write_local ~recv_peer
    ~write_peer ~path:uri ~hostname

let unix_select
    (read_handles:unix_handle list as 'lst)
    (write_handles: 'lst )
    (except_handles: 'lst)
  : 'lst * 'lst * 'lst =
  (* TODO raises EINVAL when a closed socket is used.
      - Why the fuck doesn't it report *which* fd?
     TODO if -1 == uerror("select", Nothing);
     TODO how to handle timeout?
  *)
  let read = List.rev_map (fun r -> !(r.peer_fd)) read_handles in
  let write = List.rev_map (fun r -> !(r.peer_fd)) write_handles in
  let except = List.rev_map (fun r -> !(r.peer_fd)) except_handles in
  match retry_signals (fun () -> Ofetch_wrap.select read write except) with
    | r, w, e ->
      let get_handle needle haystack =
        List.find (fun {peer_fd ; _ } -> Ofetch_wrap.equal !peer_fd needle)
          haystack in
      let handles_of_peer_fds fds haystack =
        List.rev_map (fun fd -> get_handle fd haystack) fds in
      (handles_of_peer_fds r read_handles),
      (handles_of_peer_fds w write_handles),
      (handles_of_peer_fds e except_handles)

module DomainLRU : sig
  val find_opt : string -> Unix.inet_addr option
  val add : string -> Unix.inet_addr -> unit
end = struct
  let max_lru_entries = 1000
  module Weight = Map.Make(struct
      let compare : int -> int -> int = compare
      type t = int end)
  module Domains = Map.Make(String)
  type t = { mutable w : string Weight.t ;
             mutable d : (Weight.key * Unix.inet_addr) Domains.t ;
             mutable generation : int ;
           }
  let t = { w = Weight.empty ; d = Domains.empty ; generation = 0 }
  let find_opt domain =
    match Domains.find_opt domain t.d with
    | None -> None
    | Some (old_weight, addr) ->
      (* update generation/weight to reflect that it was recently used: *)
      t.generation <- succ t.generation ;
      t.w <- Weight.remove old_weight t.w ;
      t.w <- Weight.add t.generation domain t.w ;
      t.d <- Domains.update domain (fun _ -> Some (t.generation,addr)) t.d ;
      Some addr
  let add domain addr =
    begin match Domains.find_opt domain t.d with
    | None when Weight.cardinal t.w > max_lru_entries ->
      (* we're going to add a new domain, but the cache is full,
         so we evict the least recently used: *)
        let weight, expired_domain = Weight.min_binding t.w in
        t.w <- Weight.remove weight t.w ;
        t.d <- Domains.remove expired_domain t.d
    | None -> ()
    | Some (weight, _) ->
      (* remove old weight since it will be updated with the current
         generation below. No need to touch Domains, since that will be
         overriden regardless: *)
      t.w <- Weight.remove weight t.w
    end ;
    t.generation <- succ t.generation ;
    t.w <- Weight.add t.generation domain t.w ;
    t.d <- Domains.add domain (t.generation, addr) t.d
end

let resolve_hostname hostname =
  match Unix.inet_addr_of_string hostname with
  | ip -> Ok ip
  | exception _ ->
    begin match DomainLRU.find_opt hostname with
    | Some addr -> Ok addr
    | None ->
      begin match (UnixLabels.gethostbyname hostname).Unix.h_addr_list.(0) with
        | exception _ -> Error ("unable to resolve: " ^ hostname)
        | addr -> DomainLRU.add hostname addr ; Ok addr
      end
    end

let () =
  Printexc.record_backtrace true ; (* TODO *)

  if not !Sys.interactive then (* <-- Don't parse cmdline args in utop *)
    begin match
        (* set up signal handling:*)
        ignore Unix.(sigprocmask SIG_BLOCK [Sys.sigpipe]);
        Sys.(set_signal sigusr1
               (Signal_handle
                  (fun _sig -> Printf.eprintf "TODO print some state info\n%!")
               ) ) ;

        let argv = ref (Array.to_list Sys.argv) in

        let argflag f = argv := List.filter (fun v -> not @@ f v) !argv in

        argflag (function | "-v" -> Ofetch.ofetch_global_debugging := true; true
                          | _ -> false) ;

        if List.length !argv <> 3 then begin
          Printf.eprintf
            "Usage: %s [-v] <LOCAL-FILENAME> <URL>\n%!" Sys.argv.(0) ;
          exit 1
        end ;

        let local_filename = List.nth !argv 1 in
        let url = List.nth !argv 2 in

        let open Ofetch in
        urlparse url >>= fun (protocol, hostname, port, uri) ->
        debug (fun m -> m"urlparse: protocol: %s :// %S : %d %S\n%!"
                  protocol hostname port uri);

        resolve_hostname hostname >>= fun addr ->

        mkconn ~protocol
          ~addr ~hostname ~port ~uri ~local_filename
        >>= fun request ->
        (*mkconn ~protocol ~addr ~hostname ~port ~uri
          ~local_filename:(local_filename ^ ".2") >>= fun request2 ->*)
        fetch_select ~equal:(fun a b ->
            Ofetch_wrap.equal !(a.peer_fd) !(b.peer_fd))
          ~requests:[request (*; request2*)] ~select:unix_select
      with
      | Ok () -> exit 0 (* TODO sync file buffers, close files *)
      | Error x -> (Printf.eprintf "%s: error: %s\n%!" Sys.argv.(0) x; exit 1)
    end
