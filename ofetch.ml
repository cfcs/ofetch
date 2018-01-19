(* TODO
  - test suite
  - https://tools.ietf.org/html/rfc7230#section-6.3.2
  - EINTR, EAGAIN, EWOULDBLOCK, etc. ?
  - multiple chunks in parallel
  - multiple files in parallel
  - warn on not enough disk space
  - mmap using Bigarray.*.map_file
    - use 2-dimensial bigarray for parallelized fetches to contain
      writes to allocated chunks
  - Accept-*: ?
  - Range:
    - Look for: "Accept-Ranges: bytes"
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

let substr_equal ?(off=0) (haystack:bytes) ?(len=Bytes.length haystack) needle =
  let effective_len = (min len @@ Bytes.length haystack) in
  if off < 0 || len < 0 || min (len - off) (String.length needle) <= 0
  then false else
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
type 'io data_cb_return =
  ('io request (* <-- new state*)
   * (('io request -> 'io request) option)
     (* ^ -- filter all other download states*)
  , string) result
and 'io data_cb =
  'io request -> 'io data_cb_return
and 'io request  =
  { io : 'io ;
    buf : Bytes.t ;
    buflen : int ;
    headers : Bytes.t ;
    header_off : int ref ;
    chunked_buf : ChunkedLength.t ;
    saved_bytes : int ref ;
    state : download_state ;
    read_cb : 'io data_cb option ;
    write_cb : 'io data_cb option ;
  }

let fetch_request
    ~(write_peer: string -> int -> int -> int)
     ~path ~hostname (req: 'handle request) : 'handle data_cb_return =
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
  debug "sending request to peer.\n%!";
  res_assert "unable to send HTTP request"
    (String.length buf = write_peer buf 0 (String.length buf)
    ) >>| fun () -> {req with write_cb = None}, None


type content_range =
  { first : int64 ;
    last : int64 ;
    complete : int64 option ;
  }

type content_range_response =
  | Content_range of content_range
  (* ^-- "bytes 1-4/5" = { first = 1; last = 4; total = 5} *)
  | Unsatisfiable_range of int64 (* "bytes */123" = Unsatifiable_range 123_L *)

let pp_content_range (fmt : Format.formatter) (cr : content_range_response) =
  let or_star = function None -> "*" | Some x -> Int64.to_string x in
  match cr with
  | Unsatisfiable_range complete -> Format.fprintf fmt "bytes */%Ld" complete
  | Content_range cr -> Format.fprintf fmt "bytes %Ld-%Ld/%s"
                          cr.first cr.last (or_star cr.complete)

let parse_content_range str : (content_range_response, string) result =
  (* this function receives the trimmed string after ':' *)
  (* user should verify that status code is 206 (accepted) or 416 (rejected)*)
  (* TODO we don't handle multiple ranges since servers shouldn't send them
          when we don't request them. *)
  let int_of_string_result s = match Int64.of_string s with
    | exception _ -> Error ("int_of_string_opt: " ^ s )
    | i -> Ok i
  in
  match String.(trim str |> split_on_char ' ') with
  | "bytes"::ranges::[] ->
    let construct_if_good range complete =
      begin match String.(trim range |> split_on_char '-') with
        | num1::num2::[] ->
          int_of_string_result num1 >>= fun first ->
          int_of_string_result num2 >>| fun last -> (first, last)
        | _ -> Error "unable to parse byte range"
      end >>= fun (first, last) ->
      (* A Content-Range field value is invalid if it contains a
         byte-range-resp that has a last-byte-pos value less than its
         first-byte-pos value or a complete-length value less than or equal
         to its last-byte-pos value: *)
      res_assert "Content-Range response inconsistent."
        (not ( last < first
               || (match complete with
                   | None -> false
                   | Some comp -> comp <= last || comp < 0_L)
               ||  first < 0_L)) >>| fun () ->
      Content_range {first;last;complete}
    in
    begin match String.(trim ranges |> split_on_char '/') with
      | exception _ -> Error "{Content-Range: bytes} - exception"
      | "*"::[total] ->
        int_of_string_result total >>= fun complete ->
        res_assert "Unsatisfiable Content-Range [complete] is positive"
          (complete >= 0_L) >>| fun () ->
        Unsatisfiable_range complete
      | [range]
      | [ range ; ("*"|"") ] -> construct_if_good range None
      | [ range ; total ] ->
        int_of_string_result total >>= fun complete ->
        construct_if_good range (Some complete)
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
  | None, None -> Ok Version_1_0 (* default to just looking for \r\n\rn *)
  | _ -> Error "unable to parse headers; found multiple incompatible encodings"

let fetch_download ~write_local ~recv_peer
    ( { buf ; headers; header_off; chunked_buf ; saved_bytes ;
        state ; read_cb = _ ;
        buflen ; write_cb = _ ; io : 'handle = _ (*TODO*)
      } as request)
  : 'handle data_cb_return =

  let save_data ~off chunk_len =
    let written = write_local buf off chunk_len in
    saved_bytes := !saved_bytes + written ;
    debug "save_data: off %d chunk_len %d written: %d: \
           \x1b[31m%S\x1b[0m\n%!" (* <-- color data red *)
      off chunk_len written @@ Bytes.sub_string buf off chunk_len ;
    assert(written = chunk_len)
    (* TODO this ought to return true (unless out of quota/disk space) since our
       data files are not opened O_NONBLOCK. that is however an assumption that
       is far from general. we should handle partial writes (or error).
       also, there's EINTR|EAGAIN etc to handle.
    *)
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
          debug "second i: %d saved_bytes: %d\n%!" second_i !saved_bytes ;
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
  let len_read = recv_peer buf 0 buflen in
  read_and_handle_bytes state 0 len_read >>| begin function
    | Done -> {request with state = Done; read_cb = None }, None
    | new_state -> { request with state = new_state} , None
  end

let new_request ~connection_handle ~buflen ~write_local
    ~recv_peer ~write_peer ~path ~hostname =
  { io = connection_handle ;
    buflen ;
    buf = Bytes.make (buflen + 8192) '\x00' ;
    headers = Bytes.make (buflen + 8192) '\x00' ;
    header_off = ref 0 ;
    chunked_buf = ChunkedLength.create () ;

    (* TODO this should be used for error reporting and statistics: *)
    saved_bytes = ref 0 ;

    state = Reading_headers ;
    read_cb =  Some (fetch_download ~recv_peer ~write_local) ;
    write_cb = Some (fetch_request ~write_peer ~path ~hostname) ;
  }

(* download:
   - callback for examining headers
     - able to issue new request (302 forwarded, 416 -> 206)
     - able to cancel downloading/triggering teardown of socket (404, 416)
       - reschedule on new socket
     - able to influence where stuff is written? 206 Partial)
   request:
   - cancel downloading (404, 416)
   request_id:
   - opaque type passed to cb, allow failing all downloads of this resource?
   cancel all outstanding requests that match *
   - provide callback on each request object
*)

let fetch_select (type fd)
    ~(requests : fd request list)
    ~(select: fd list -> fd list -> fd list ->
      fd list * fd list * fd list)
  : (unit, string) result =
    (*let module OrderedFD : Set.OrderedType with type t = fd request =
      struct
      type t = fd request
      let compare a b = compare a.io b.io
      end
      in
      let module FDSet = Set.Make(OrderedFD) in*)
  let rec select_loop ~(requests:fd request list) =
    debug "at select_loop with %d requests\n%!" @@ List.length requests ;
    let map_if_some ~default f = function None -> default | Some v -> f v in
    let readable , writeable, exceptfds =
      let fold f =
        List.fold_left (fun acc r ->
            if f r <> None then r.io::acc else acc) [] requests in
      let readable = fold (fun r -> r.read_cb)
      and writeable = fold (fun r -> r.write_cb) in
      select readable writeable []
    in
    (* TODO should be a set of requests ordered by fd rather than a list *)
    let rec do_io errors (acc : fd request list)
      : fd request list -> string list * fd request list =
      begin function
        | [] -> errors, acc
        | req::tl ->
          let in_set ~req set cb =
            map_if_some ~default:(Ok req)
              (fun io_cb -> io_cb req >>| fun (new_req, _f_cb) -> new_req)
              (if List.exists (fun fd -> fd = req.io) set then cb else None) in
          let null req = {req with read_cb = None ; write_cb = None} in
          begin match in_set ~req readable req.read_cb with
            | Error err -> do_io (err::errors) (null req::acc) tl
            | Ok read_req ->
              begin match in_set ~req:read_req writeable read_req.write_cb with
                | Error err -> do_io (err::errors) (null read_req::acc) tl
                | Ok written_req -> do_io errors (written_req::acc) tl
              end
          end
      end
    in
    let errors, (inactive, live_requests) =
      let errors, new_states = do_io [] [] requests in
      let()= assert(List.length errors <= List.length requests) in
      let() =assert(List.length new_states <= List.length requests) in
      errors,
      List.map (fun old ->
          (* update with new state if applicable: *)
          try List.find (fun n -> n.io = old.io) new_states
          with Not_found -> old
        ) requests
      |> List.partition (fun r -> r.read_cb = None && r.write_cb = None)
    in
    let completed, failed = List.partition (fun r -> r.state = Done) inactive in
    debug "errs: %d initial reqs: %d live requests: %d \
           completed: %d  failed: %d\n%!"
      (List.length errors) (List.length requests) (List.length live_requests)
      (List.length completed) (List.length failed)
    ;
    List.iter (fun err -> Printf.eprintf "request error: %s\n%!" err) errors ;
    (* TODO expose errors in return code *)
    if [] <> live_requests
    then (select_loop[@tailcall]) ~requests:live_requests
    else (if [] = errors then Ok (debug "all good\n")
          else Error "exiting with errors")
  in
  debug "STARTING MAIN LOOP WITH %d requests\n%!" @@ List.length requests ;
  select_loop ~requests

let urlparse str =
  Ok (String.split_on_char '#' str |> List.hd) >>= fun str ->
  res_assert (Format.asprintf "url %S doesn't start with 'http://'" str)
    (substr_equal ~off:0 (Bytes.of_string str) "http://" )
   >>| (fun () -> String.sub str 7 (String.length str - 7)) >>= fun str ->
  match String.index str '/' with
  | exception Not_found -> Ok (str, 80, "/")
  | i ->
    begin match String.(sub str 0 i |> split_on_char ':') with
      | host::port::[] ->
        begin match int_of_string port with
          | exception (Failure _) -> Error "urlparse: invalid port"
          | port when port < 0 || 0xFFFF < port ->
            Error "urlparse: invalid port range"
          | port -> Ok (host, port)
        end
      | [host] -> Ok (host, 80)
      | _ -> Error ("wtf is this host:port tuple" ^ str)
    end >>| fun (host,port) ->
    (host, port,
     String.sub str i (String.length str - i))
