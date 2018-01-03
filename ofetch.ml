(* TODO
  - test suite
  - https://tools.ietf.org/html/rfc7230#section-6.3.2
  - EINTR, EWOULDBLOCK, etc. ?
  - multiple chunks in parallel
  - multiple files in parallel
  - warn on not enough disk space
  - mmap using Bigarray.*.map_file
    - use 2-dimensial bigarray for parallelized fetches to contain
      writes to allocated chunks
  - Accept-*: ?
  - Range:
    - Accept-Ranges: bytes
    - Content-Range
      Where in a full body message this partial message belongs
      - Content-Range: bytes 21010-47021/47022
    - https://en.wikipedia.org/wiki/Byte_serving
  - https://en.wikipedia.org/wiki/List_of_HTTP_header_fields
*)

let ofetch_global_debugging = ref false (* <-- print debug messages to stderr *)

let debug ?(fd=stderr) =
  if !ofetch_global_debugging
  then Printf.fprintf fd
  else Printf.ifprintf fd

type response_transfer_method =
  | Content_length of int
  | Chunked of int
  | Version_1_0

type download_state =
  | Reading_headers
  | Reading_body of response_transfer_method
  | Done

let (>>=) a b = (* Rresult.(>>=) *)
  match a with
  | Ok x -> b x
  | Error _ as err -> err

let (>>|) a b = (* Rresult.(>>|) *)
  a >>= (fun x -> Ok (b x))

let res_assert msg = function | true -> Ok ()
                              | false -> Error ("assertion failed: " ^ msg)

let fetch_request (type fd)
    (write_sub:fd -> string -> int -> int -> int)
    (fd:fd) ~path ~hostname
    () =
  let buf =
    String.concat "\r\n"
      [ "GET " ^ path ^ " HTTP/1.1" ;
        "Host: " ^ hostname ;
        "DNT: 1 (Do Not Track Enabled)" ;
        "Connection: close" ;
        "Accept-Encoding: identity" ;
        "TE: " ;  (* <--^-- please no gzip, RFCs 7231:5.3.4 + 7230:4.3 *)
        "\r\n" (* <-- ensure trailing \r\n *)
      ]
      (* Range: xxx-xxx/yyy
         If-Range: Wed, 15 Nov 1995 04:58:08 GMT
         If-Unmodified-Since: https://tools.ietf.org/html/rfc7232#section-3.4
           - 412 (Precondition Failed)
      *)
  in
  res_assert "unable to send HTTP request"
    (String.length buf = write_sub fd buf 0 (String.length buf)
    ) >>| fun _ -> Done

let substr_equal ?(off=0) (haystack:bytes) ?(len=Bytes.length haystack) needle =
  let effective_len = (min len @@ Bytes.length haystack) in
  if max 1 @@ (max 0 off) + (max 1 @@ String.length needle) > effective_len
  then false
  else Bytes.sub_string haystack off (String.length needle) = needle

let index_substr ?(off=0) haystack ?(len=Bytes.length haystack) substr
  : int option =
  if off + String.length substr > min len (Bytes.length haystack) then None
  else if substr = "" then Some off else
  let fchar = substr.[0] in (* length of substr must be at least one *)
  let rec find offset : int option =
    let offset = max 0 offset in (*TODO decide whether this is the best option*)
    let off = Bytes.index_from haystack offset fchar in
    if off + String.length substr > len then
      None
    else
      ( if substr_equal ~off haystack substr
        then Some off
        else (find[@tailcall]) (succ off) )
  in try find off with Not_found -> None

let parse_content_range str
  (* start-offset  end-offset    total-size  error-msg *)
  : ((int option * int option ) * int option, string) result =
  (* this function receives the trimmed string after ':' *)
  (* user should verify that status code is 206 (accepted) or 416 (rejected)*)
  (* TODO we don't handle multiple ranges since servers shouldn't send them
          when we don't request them. *)
  let int_of_string_result s = match int_of_string s with
    | exception _ -> Error ("int_of_string_opt: " ^ s )
    | i -> Ok i
  in
  let dash_range s : (int option * int option, string) result =
    match String.(trim s |> split_on_char '-') with
    | ["*"] -> Ok (None, None)
    | num1::("" | "*")::[] ->
      int_of_string_result num1 >>= fun start ->
      res_assert "Content-Range: start must be positive"
        (start >= 0) >>| fun () -> (Some start, None)
    | ("" | "*")::num2::[] ->
      int_of_string_result num2 >>= fun upper_bound ->
      res_assert "Content-Range: upper_bound must be positive"
        (upper_bound >= 0) >>| fun () -> (None, Some upper_bound)
    | num1::num2::[] ->
      int_of_string_result num1 >>= fun start ->
      int_of_string_result num2 >>= fun upper_bound ->
      res_assert "Content-Range: inconsistent range"
        (start >= 0 && upper_bound >= start) >>| fun () ->
      (Some start, Some upper_bound)
    | _ -> Error "unable to parse byte range"
  in
  match String.(trim str |> split_on_char ' ') with
  | "bytes"::ranges::[] ->
    begin match String.(trim ranges |> split_on_char '/') with
      | exception _ -> Error "{Content-Range: bytes} - exception"
      | [range]
      | [ range ; ("*"|"") ] -> dash_range range >>| fun x -> (x, None)
      | [ range ; total ] ->
        int_of_string_result total >>= fun total ->
        dash_range range >>= begin function
          | Some x, _ when (x >= total || total < 0) ->
            Error "Content-Range: range/total inconsistent [lower]"
          | _, Some x when (x >= total || total < 0) ->
            Error "Content-Range: range/total inconsistent [upper]"
          (* must be 0-(total-1)/total : *)
          | range -> Ok (range, Some total)
        end
      | _ -> Error "{Content-Range: bytes} - unable to parse"
    end
  | _ -> Error "{Content-Range: } - doesn't use {bytes}"

let parse_headers str =
  (*An origin server that supports byte-range requests for a given target
   resource MAY send Accept-Ranges: bytes*)
  (*  A server that does not support any kind of range request for the
      target resource MAY send Accept-Ranges: none*)
  (* TODO see test suite*)
  (* TODO consider ETag parsing: https://tools.ietf.org/html/rfc7232#section-2.3*)
  let headers = String.(split_on_char '\n' str
                        |> List.map (fun x -> trim x |> lowercase_ascii))
  in
  let find_header str =
    try Some (List.find (fun x -> substr_equal (Bytes.of_string x) str) headers)
    with Not_found -> None
  in
  match find_header "content-length:", find_header "transfer-encoding:" with
  | None, Some _chunked -> Ok (Chunked 0)
  | Some ctl, None ->
    begin match String.split_on_char ':' ctl with
      | "content-length"::tl::_ ->
        Ok (Content_length (int_of_string (String.trim tl)))
      | _ -> Error "TODO handle invalid content-length"
    end
  | _ -> Ok Version_1_0 (* default to just looking for \r\n\rn *)

module ChunkedLength :
sig
  type t
  val create : unit -> t
  val add : t -> bytes -> int -> int -> (t, string) result
  val to_int : t -> (int, string) result
  val contains : t -> string -> bool
end =
struct
  type t = Buffer.t

  let create () = Buffer.create 8

  let add t bytes off len =
    if Buffer.length t + len > 7 then (
      debug "ChunkedLength.add -> contents: %S\n\t\tadding: %S\n%!"
        (Buffer.contents t)
        (Bytes.sub_string bytes off len) ;
      Error "Transfer-Encoding: chunked: more than 4 bytes" )
    else
      ( Buffer.add_subbytes t bytes off len ; Ok t )

  let contains t needle = None <> index_substr (Buffer.to_bytes t) needle

  let to_int t =
    debug "ChunkedLength.to_int %S\n%!" @@ Buffer.contents t;
    try Ok ( let i = "0x" ^ (Buffer.contents t |> String.trim) |> int_of_string
             in Buffer.clear t ; i )
    with Invalid_argument _ -> Error "ChunkedLength.t contains invalid data"

end

let fetch_download ~buflen ~write_local ~recv_peer
  : unit -> (download_state, string) result =
  let state = ref Reading_headers in

  let buf = Bytes.make (buflen + 8192) '\x00' in

  let headers = Bytes.make (buflen + 8192) '\x00' in
  let header_off = ref 0 in

  let chunked_buf = ChunkedLength.create () in

   (* TODO this should be used for error reporting and statistics: *)
  let saved_bytes = ref 0 in

  let save_data ~off chunk_len =
    let written = write_local buf off chunk_len in
    saved_bytes := !saved_bytes + written ;
    debug "save_data: off %d chunk_len %d written: %d: \
           \x1b[31m%S\x1b[0m\n%!" (* <-- color data red *)
      off chunk_len written @@ Bytes.sub_string buf off chunk_len ;
    assert(written = chunk_len) (* TODO *)
  in

  let fst_unless_equal a b = if a-b <> 0 then Some a else None in

  let state_reading_headers ~buf_offset len
    : (download_state * int option, string) result=
    res_assert "not enough space for headers"
      (!header_off + len < buflen) >>= fun () ->
    Bytes.blit buf buf_offset headers !header_off len ;
    (*substr_equal ~off:0 "HTTP/1.1 200" buf TODO for normal stuff*)
    (*substr_equal ~off:0 "HTTP/1.1 206" buf TODO for partial/Range:*)
    let old_header_off = !header_off in
    header_off := old_header_off + len ;
    let two_newlines = "\r\n\r\n" in
    begin match index_substr ~off:(old_header_off-5)
                  ~len:!header_off headers two_newlines
      with
      | Some end_of_headers ->
        let heads = Bytes.sub_string headers 0 end_of_headers in
        debug "received %d headers [len %d]: %S\n%!"
          (String.length heads) len heads ;
        parse_headers heads >>= fun parsed_headers ->
        (* compute difference between last offset (!header_off)
           and the end of headers found in that. TODO
        *)
        let body_offset = end_of_headers - old_header_off
                          + String.length two_newlines in
        debug "cool we're done with headers: \
               buf_offset: %d len: %d body_offset: %d\n%!"
          buf_offset len body_offset ;
        assert (0 <= body_offset && body_offset <= len);
        Ok (Reading_body parsed_headers, Some body_offset)
      (* ^-- we deliberately don't use fst_unless_equal
             here because a 0-length body is allowed. *)
      | None ->
        Ok (Reading_headers, None) (* still no consecutive newlines found *)
    end
  in
  let state_reading_response ~off len
    : response_transfer_method -> (download_state * int option, string) result =
    let ok_more state offset =
      Ok (Reading_body state, fst_unless_equal offset len)
    in
    function
    | Version_1_0 as current_state ->
      (* TODO consider adding switch to download until end of stream *)
      begin match index_substr ~off buf ~len "\r\n\r\n" with
        | Some i -> (* end of download *)
          save_data ~off (i-off) ;
          Ok (Done, None)
        | None ->
          save_data ~off (len-off) ;
          ok_more current_state len
      end

    | Content_length missing_len ->
      let this_slice = min (len-off) missing_len in
      let remaining_of_download = missing_len - this_slice in
      debug "content-length: this_slice: %d len: %d off: %d missing_len: %d \
             remaining_of_download(after): %d\n%!"
        this_slice len off missing_len remaining_of_download ;
      save_data ~off (this_slice) ;
      assert (remaining_of_download >= 0) ;
      if remaining_of_download = 0 then
        Ok (Done, Some this_slice)
      else
        ok_more (Content_length remaining_of_download) len

    | Chunked 0 (* Some 0: remaining is 0 *)
      -> (* We're not currently reading data from a chunk;
            we are trying to parse a chunk len: *)
      begin match index_substr ~off buf ~len "\n" with (*TODO look for \r *)

        | None -> (* no newline was found, so we add everything to len buffer:*)
          debug "Adding everything to chunked_buf\n%!" ;
          ChunkedLength.add chunked_buf buf off (len - off) >>= fun _ ->
          ok_more (Chunked 0) len

        | Some second_i when ChunkedLength.contains chunked_buf "\n"
                          || !saved_bytes = 0 (* OR if this is the first chunk *)
          -> (*we have found the second \n, so now we parse the chunk length: *)
          ChunkedLength.(add chunked_buf buf off (second_i - off) >>= to_int)
          >>= begin function
            | 0 -> Ok (* when we *receive* len 0, that's EOF: *)
                     (Done, Some (second_i+1))
            | chunk_marker -> ok_more (Chunked chunk_marker) (second_i + 1)
          end

        | Some first_i -> (* found first newline of \r\nXXXX\r\n *)
          begin match index_substr ~off:(first_i+1) buf ~len "\n" with
            | None ->
              let len_of_marker = len-first_i in
              let new_offset = first_i + len_of_marker in
              assert(len = new_offset) ;
              debug "Found first newline: off: %d first_i: %d\
                     len_of_marker: %d new_offset: %d\n%!"
                off first_i len_of_marker new_offset ;
              ChunkedLength.add chunked_buf buf first_i len_of_marker
              >>= fun _ -> ok_more (Chunked 0) new_offset

            | Some second_i -> (* second newline found of \r\nXXXX\r\n *)
              debug "now we have a chunk at off: %d i: %d len: %d\n%!"
                off second_i len;
              ChunkedLength.(add chunked_buf buf off (max 0 (second_i-off))
                             >>= to_int) >>= fun chunk_marker ->
              let new_offset = (second_i+1) in (* 1: '\n'*)
              debug "--FOUND CHUNK MARKER at off %d len: %d: \
                     chunk_marker: %d new_off: %d\n%!"
                off len
                (chunk_marker) new_offset ;
              if 0 = chunk_marker then begin
                Ok (Done, None) (* no more content to be read *)
              end else
                ok_more (Chunked chunk_marker) new_offset
          end
      end

    | Chunked chunk_len -> (* We already have a chunk, download data:*)
      let this_chunk = min chunk_len (len-off) in
      let new_offset = (off + this_chunk) in
      debug "READ CHUNK len: %d off: %d, %d/%d [new_offset: %d]: %S\n%!"
        len off
        this_chunk chunk_len
        new_offset (Bytes.sub_string buf off this_chunk) ;
      save_data ~off this_chunk ;
      ok_more (Chunked (chunk_len - this_chunk)) new_offset
  in
  let rec read_and_handle_bytes state (buf_offset:int) n =
    match state, n with
    | Done , _ -> Ok Done (* TODO ignore extraneous stuff? *)
    | _,  0 -> Error "unexpected end of stream"
    | Reading_headers, len ->
      state_reading_headers ~buf_offset len >>= begin function
        | Done, None -> Ok Done
        | (Reading_headers | Reading_body _ as rb) , None -> Ok rb
        | (Reading_body _ | Done | Reading_headers as state), Some new_offset ->
          (read_and_handle_bytes[@tailcall]) state new_offset n
      end
    | Reading_body transfer_method, len ->
      state_reading_response ~off:buf_offset len transfer_method
      >>= begin function
        | Reading_headers, _ ->
          Error "illegal state transition from body -> header parsing"
        | (Done | Reading_body _ as state), None -> Ok state
        | (Done | Reading_body _ as state), Some new_offset ->
          (read_and_handle_bytes[@tailcall]) state new_offset n
      end
  in
  fun () -> (* return a [unit -> ..] function to be called from fetch_select *)
    let len_read = recv_peer buf 0 buflen in
    read_and_handle_bytes !state 0 len_read >>| (fun new_state ->
        (* save the state machine index before returning: *)
        state := new_state; !state)

let fetch_select
    ~(read_lst:('fd * (unit -> (download_state, string) result)) list as 'io_cb)
    ~(write_lst:'io_cb) ~(select: 'fd list -> 'fd list -> 'fd list ->
                 'fd list * 'fd list * 'fd list) : (unit, string) result =
  let rec loop ~read_fds ~write_fds =
    let readable, writable, exceptional_fds =
      select read_fds write_fds []
    in
    let partition fds lookup = (* TODO should probably just return failed *)
      List.map (fun fd ->
          let cb = List.assoc fd lookup in
          fd, cb ()
        ) fds
      |> List.partition
        (function | _, Ok Done -> false (* remove Done states *)
                  | _, Ok (Reading_body _ | Reading_headers) -> true
                  | _, Error _ -> false)
    in
    let write_fds =
      let ok , failed = partition writable write_lst in
      let failed_fds, _ = List.split failed in
      List.filter (fun fd -> not @@ List.mem fd failed_fds) write_fds
    in
    let read_fds =
      let ok , failed = partition readable read_lst in
      let failed_fds, _ = List.split failed in
      List.iter (function
          | _fd, Ok Done -> debug "Fetch succeeded\n%!"
          | _fd, Error msg -> debug "ERR: %S\n%!" msg
          | _ -> () (*TODO*)
        ) failed ;
      List.filter (fun fd -> not @@ List.mem fd failed_fds) read_fds
    in
    if read_fds <> [] then (loop[@tailcall]) ~read_fds ~write_fds
    else Ok ()
  in
  let write_fds, _ = List.split write_lst
  and read_fds , _ = List.split read_lst in
  loop ~read_fds ~write_fds

let urlparse str =
  Ok (String.split_on_char '#' str |> List.hd) >>= fun str ->
  res_assert (Format.asprintf "url %S doesn't start with 'http://'" str)
    (substr_equal ~off:0 (Bytes.of_string str) "http://" )
   >>| (fun () -> String.sub str 7 (String.length str - 7)) >>= fun str ->
  match String.index str '/' with
  | exception Not_found -> Ok (str, 80, "/")
  | i ->
    begin match String.(sub str 0 i |> split_on_char ':') with
      | host::port::[] -> Ok (host, (int_of_string port)) (* TODO *)
      | [host] -> Ok (host, 80)
      | _ -> Error ("wtf is this host:port tuple" ^ str)
    end >>| fun (host,port) ->
    (host, port,
     String.sub str i (String.length str - i))
