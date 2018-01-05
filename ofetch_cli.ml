open Ofetch

let unix_write fd buf pos len = UnixLabels.write_substring fd ~buf ~pos ~len

let try_chroot () =
  let open Unix in
  try chroot "." ; chdir "/"
  with Unix_error (EPERM, _, _ ) -> ()

let fetch_conn inet_addr port =
  try_chroot () ;
  let fd = Unix.(socket PF_INET SOCK_STREAM 0) in (* 4.05 introduces new args.*)
  let open UnixLabels in
  set_nonblock fd ;
  let addr = ADDR_INET (inet_addr, port) in
  ( try connect fd ~addr with Unix_error (EINPROGRESS, _, _ ) -> () ) ; fd

let open_new_file ~filename =
  try Ok (Unix.openfile filename Unix.[O_CREAT ; O_WRONLY; O_EXCL] 0o600)
  with
  | Unix.(Unix_error (a, b, c)) -> Error (a,b,c)

let mkconn ~(addr:Unix.inet_addr) ~hostname ~port ~uri ~local_filename =
  let fd = fetch_conn addr port in
  let request_cb = fetch_request unix_write fd ~path:uri ~hostname in
  begin match open_new_file ~filename:local_filename with
    | Ok fd -> Ok fd
    | Error (Unix.EEXIST, "open", fn) ->
      Error (Format.asprintf "Output file %S already exists." fn)
    | Error _ -> Error "unix error while opening file"
  end >>| fun file_channel ->
  let mmap fd =
    (*let open Unix.LargeFile in*)
    let open Bigarray in
    (*fstat  http://caml.inria.fr/pub/docs/manual-ocaml/libref/UnixLabels.LargeFile.html *)
    let seek = 0L and shared = true and dimensions = (-1) in
    Bigarray.Array1.map_file fd ~pos:seek Char C_layout shared dimensions
  in
  let buflen = UnixLabels.(getsockopt_int fd SO_RCVBUF) in
  let write_local buf pos len = UnixLabels.write file_channel ~buf ~pos ~len in
  let recv_peer buf pos len = UnixLabels.read fd ~buf ~pos ~len in
  let download_cb = fetch_download ~buflen ~write_local ~recv_peer in
  (fd, download_cb), (fd, request_cb)

let unix_select read write except =
  UnixLabels.select ~read ~write ~except ~timeout:45.0

let () =
  Printexc.record_backtrace true ; (* TODO *)

  if not !Sys.interactive then (* <-- Don't parse cmdline args in utop *)
    begin match
        let argv = ref (Array.to_list Sys.argv) in

        let argflag f = argv := List.filter (fun v -> not @@ f v) !argv in

        argflag (function | "-v" -> ofetch_global_debugging := true; true
                          | _ -> false) ;

        if List.length !argv <> 3 then begin
          Printf.eprintf
            "Usage: %s [-v] <LOCAL-FILENAME> <URL>\n" Sys.argv.(0) ;
          exit 1
        end ;

        let local_filename = List.nth !argv 1 in
        let url = List.nth !argv 2 in

        urlparse url >>= fun (hostname, port, uri) ->
        debug "urlparse: %S : %d %S\n%!" hostname port uri;
        (* let addr = Unix.inet_addr_of_string ip_v4_str in *)
        let addr = (UnixLabels.gethostbyname hostname).Unix.h_addr_list.(0) in

        mkconn ~addr ~hostname ~port ~uri ~local_filename >>= fun (rcv,snd) ->
        fetch_select ~read_lst:[rcv] ~write_lst:[snd] ~select:unix_select
      with
      | Ok () -> exit 0 (* TODO sync file buffers, close files *)
      | Error x -> (Printf.eprintf "%s: error: %s\n%!" Sys.argv.(0) x; exit 1)
    end
