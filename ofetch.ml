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

module type Peer_S =
sig
  type t

  type init_data

  val init : protocol:string -> init_data -> (t, string) result

  val select : t list -> t list -> t list -> t list * t list * t list
  (** TODO also need a way to signal state updates, even to fds that
     are not to be read/written (use exceptfds for this?)
  *)

  val recv_peer : t -> buf:bytes -> pos:int -> len:int ->
    (int * t, string) result
  (** [recv_peer state ~buf ~pos ~len] reads up to [len] bytes from [state]
      into [buf] at offset [pos].
      The returned value is either
      - [Ok (amount_read, next_state)] where [amount_read] is the actual amount
      of bytes read, which may be lower than [len].
      - [Error remaining] where [remaining] is the remaining input buffer.
  *)

  val write_peer : t -> buf:string -> pos:int -> len:int -> int * t
  (** write a substring to a peer (or buffer it).
     TODO interface should be changed to allow error handling. *)

  val equal : t -> t -> bool
  (** bool: whether the two t operate on the same sockets *)
end

let ofetch_global_debugging = ref false (* <-- print debug messages to stderr *)

let debug ?(fd=stderr) f : unit =
  if !ofetch_global_debugging
  then f (Printf.fprintf fd)
  else ()

type response_transfer_method =
  | Content_length of int
  | Chunked of int
  | Version_1_0 of int (* number of \r\n\r\n encountered at the end *)

type download_state =
  | Reading_headers
  | Reading_body of response_transfer_method
  | Done

module Results = struct
  let (>>=) a b = (* Rresult.(>>=) *)
    match a with
    | Ok x -> b x
    | Error _ as err -> err

  let (>>|) a b = (* Rresult.(>>|) *)
    a >>= (fun x -> Ok (b x))
end
open Results

let res_assert msg = function | true -> Ok ()
                              | false -> Error ("assertion failed: " ^ msg)

let substr_equal ?(off=0) (haystack:bytes) ?(len=Bytes.length haystack) needle =
  (* true if the substring of [haystack] starting at [off]
     matches [needle]. [len] is used to limit size of [needle].*)
  if off < 0 || len < 0 then false else
  let needle_len = String.length needle in
  let effective_len = (min len @@ Bytes.length haystack - off) in
  if effective_len < needle_len then false else
    let rec loop n acc =
         if n < 0 then acc
         else begin if Bytes.get haystack (off+n) = needle.[n] then
             loop (pred n) acc
           else false end
    in loop (pred needle_len) true

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
      debug (fun m -> m "ChunkedLength.add -> contents: %S\n\t\tadding: %S\n%!"
                (Buffer.contents t) (Bytes.sub_string bytes off len)) ;
      Error "Transfer-Encoding: chunked: more than 4 bytes" )
    else
      ( Buffer.add_subbytes t bytes off len ; Ok t )

  let contains t needle = None <> index_substr (Buffer.to_bytes t) needle

  let to_int t =
    debug (fun m -> m "ChunkedLength.to_int %S\n%!" @@ Buffer.contents t);
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
  let terminate = Ok ({req with write_cb = None}, None) in
  debug (fun m -> m"sending request to peer.\n%!");
  let rec write_remaining offset _ : 'handle data_cb_return =
    let missing_len = (String.length buf - offset) in
    if missing_len = 0 then begin
      debug (fun m -> m"we sent our payload\n%!");
      terminate
    end else begin
      let written = write_peer buf offset missing_len in
      debug (fun m -> m"written: off:%d written:%d (missing:%d): %S\n%!"
                offset written missing_len
                (String.sub buf offset missing_len));
      if missing_len <> written then
        Ok ({req with write_cb =
                        Some (write_remaining (written))}, None)
      else
        terminate end
  in
  write_remaining 0 req


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
                        |> List.rev_map (fun x -> trim x |> lowercase_ascii))
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
  | None, None -> Ok (Version_1_0 0) (* default to just looking for \r\n\r\n *)
  | _ -> Error "unable to parse headers; found multiple incompatible encodings"

let fetch_download ~write_local
    ~(recv_peer: Bytes.t -> int -> int ->
      (int, string) result)
    ( { buf ; headers; header_off; chunked_buf ; saved_bytes ;
        state ; read_cb = _ ; write_cb = _ ; io : 'handle = _ (*TODO*)
      } as request)
  : 'handle data_cb_return =

  let save_data ~off chunk_len =
    let written = write_local buf off chunk_len in
    saved_bytes := !saved_bytes + written ;
    debug (fun m ->
        let sub = Bytes.sub_string buf off chunk_len in
        let all_zero =
          let x = ref true in String.iteri (fun i -> function
              |'\000'->()
              |_->x:=i < off || i > off+chunk_len) sub ; !x in
        m"save_data: off %d chunk_len %d written: %d: \
                      \x1b[31m%S\x1b[0m\n%!" (* <-- color data red *)
              off chunk_len written (if all_zero then "ALL-ZERO" else sub)) ;
    assert(written = chunk_len)
    (* TODO this ought to return true (unless out of quota/disk space) since our
       data files are not opened O_NONBLOCK. that is however an assumption that
       is far from general. we should handle partial writes (or error).
       also, there's EINTR|EAGAIN etc to handle.
    *)
  in

  let fst_unless_equal a b = if a-b <> 0 then Some a else None in

  let state_reading_headers ~buf_offset len
    : (download_state * int option, string) result =
    let actually_consumed = min (Bytes.length headers - !header_off) (len)
                          |> min (Bytes.length buf - buf_offset) in
    Bytes.blit buf buf_offset
      headers !header_off
      actually_consumed ;
    (*substr_equal ~off:0 "HTTP/1.1 200" buf TODO for normal stuff*)
    (*substr_equal ~off:0 "HTTP/1.1 206" buf TODO for partial/Range:*)
    let old_header_off = !header_off in
    header_off := old_header_off + actually_consumed ;
    let two_newlines = "\r\n\r\n" in
    begin match index_substr ~off:(old_header_off-5)
                  ~len:!header_off headers two_newlines
      with
      | Some end_of_headers ->
        let heads = Bytes.sub_string headers 0 end_of_headers in
        debug (fun m -> m"received %d headers [len %d]: %S\n%!"
                  (String.length heads) len heads) ;
        parse_headers heads >>= fun parsed_headers ->
        (* compute difference between last offset (!header_off)
           and the end of headers found in that. TODO
        *)
        let body_offset = end_of_headers - old_header_off
                          + String.length two_newlines in
        debug (fun m -> m"cool we're done with headers: \
                          buf_offset: %d len: %d body_offset: %d\n%!"
                  buf_offset len body_offset) ;
        assert (0 <= body_offset && body_offset <= len);
        Ok (Reading_body parsed_headers, Some body_offset)
      (* ^-- we deliberately don't use fst_unless_equal
             here because a 0-length body is allowed. *)
      | None when !header_off = Bytes.length headers ->
        Error "not enough space for headers"
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
    | Version_1_0 0 as current_state -> (* looking for stuff at the end *)
      (* TODO consider adding switch to download until end of stream *)
      begin match index_substr ~off buf ~len "\r\n\r\n" with
        | Some i -> (* end of download *)
          save_data ~off (i-off) ;
          Ok (Done, None)
        | None when len - off = 0 ->
          ok_more current_state len
        | None ->
          let next =
            if None <> index_substr ~len ~off:(len-3) buf "\r\n\r" then 3
            else if None <> index_substr ~len ~off:(len-2) buf "\r\n" then 2
            else if None <> index_substr ~len ~off:(len-1) buf "\r" then 1
            else 0 in
          save_data ~off (len - off - next) ;
          ok_more (Version_1_0 next) len
      end
    | Version_1_0 seen -> (*looking for stuff at the beginning*)
      let can_see = max 0 (min (len - off) (4-seen)) in
      let sequence = String.sub "\r\n\r\n" seen can_see in
      begin match index_substr ~off buf ~len:(off+can_see) sequence with
        | Some 0 -> Ok (Done, None)
        | None
        | Some _ ->
          (* save_data ~off:0 ~len:String.(seen) "\r\n\r\n" *)
          ( (* we ignored some \r etc in the previous loop, but now we know
               that they were actually part of the response body: *)
            let written = write_local (Bytes.of_string "\r\n\r\n") 0 seen in
            saved_bytes := !saved_bytes + written ) ;
          (* skip one character that DIDN'T match and reset state:*)
          save_data ~off 1 ;
          ok_more (Version_1_0 0) 1
      end

    | Content_length missing_len ->
      let this_slice = min (len-off) missing_len in
      let remaining_of_download = missing_len - this_slice in
      debug (fun m ->
          m"content-length: this_slice: %d len: %d off: %d missing_len: %d \
            remaining_of_download(after): %d\n%!"
            this_slice len off missing_len remaining_of_download) ;
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
          debug (fun m -> m"Adding everything to chunked_buf\n%!") ;
          ChunkedLength.add chunked_buf buf off (len - off) >>= fun _ ->
          ok_more (Chunked 0) len

        | Some second_i when ChunkedLength.contains chunked_buf "\n"
                          || !saved_bytes = 0 (* OR if this is the first chunk *)
          -> (*we have found the second \n, so now we parse the chunk length: *)
          debug (fun m ->
              m"second i: %d saved_bytes: %d\n%!" second_i !saved_bytes) ;
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
              debug (fun m -> m "Found first newline: off: %d first_i: %d\
                                 len_of_marker: %d new_offset: %d\n%!"
                        off first_i len_of_marker new_offset) ;
              ChunkedLength.add chunked_buf buf first_i len_of_marker
              >>= fun _ -> ok_more (Chunked 0) new_offset

            | Some second_i -> (* second newline found of \r\nXXXX\r\n *)
              debug (fun m ->
                  m"now we have a chunk at off: %d i: %d len: %d\n%!"
                    off second_i len);
              ChunkedLength.(add chunked_buf buf off (max 0 (second_i-off))
                             >>= to_int) >>= fun chunk_marker ->
              let new_offset = (second_i+1) in (* 1: '\n'*)
              debug (fun m -> m"--FOUND CHUNK MARKER at off %d len: %d: \
                                chunk_marker: %d new_off: %d\n%!"
                        off len (chunk_marker) new_offset) ;
              if 0 = chunk_marker then begin
                Ok (Done, None) (* no more content to be read *)
              end else
                ok_more (Chunked chunk_marker) new_offset
          end
      end

    | Chunked chunk_len -> (* We already have a chunk, download data:*)
      let this_chunk = min chunk_len (len-off) in
      let new_offset = (off + this_chunk) in
      debug (fun m ->
          m"READ CHUNK len: %d off: %d, %d/%d [new_offset: %d]: %S\n%!"
            len off
            this_chunk chunk_len
            new_offset (Bytes.sub_string buf off this_chunk)) ;
      save_data ~off this_chunk ;
      ok_more (Chunked (chunk_len - this_chunk)) new_offset
  in
  let rec read_and_handle_bytes state (buf_offset:int) n =
    match state, n with
    | Done , _ -> Ok Done (* TODO ignore extraneous stuff? *)
    | same_state,  0 -> Ok same_state
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
  begin match recv_peer buf 0 (Bytes.length buf) with
    | Error input_buffer ->
      (* The underlying transport has stopped working, either due to network
         errors or because the server has closed the connection correctly after
         sending us the data we need to complete parsing the response.
         At this point [input_buffer] may be larger than [buf].
         We need to blit into [buf] as many times as possible and try to exhaust
         [input_buffer]: *)
      let rec loop = function
        | state, 0 -> Ok state
        | state, leftover ->
          let this = min leftover (Bytes.length buf) in
          String.blit input_buffer 0 buf 0 this ;
          read_and_handle_bytes state 0 this >>= fun state ->
          loop (state, leftover - this)
      in
      begin match loop (state, String.length input_buffer) with
        | Ok Done -> Ok ({request with state = Done; read_cb = None }, None)
        | Ok Reading_headers ->
          Error "Unexpected end of HTTP stream while reading headers"
        | Ok Reading_body _ ->
          Error "Unexpected end of HTTP stream while reading body content"
        | Error msg -> Error ("Got EOF|ALERT and triggered error: " ^ msg)
      end
    | Ok len_read ->
      read_and_handle_bytes state 0 len_read >>| begin function
        | Done -> {request with state = Done; read_cb = None }, None
        | ( Reading_headers
          | Reading_body _) as new_state ->
          { request with state = new_state} , None
      end
  end

let new_request ~connection_handle ~buflen ~write_local
    ~recv_peer ~write_peer ~path ~hostname =
  { io = connection_handle ;
    buf = Bytes.make (buflen) '\x00' ;
    headers = Bytes.make (buflen) '\x00' ;
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
    ~(equal    : fd -> fd -> bool )
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
    debug (fun m ->
        m"at select_loop with %d requests\n%!" @@ List.length requests) ;
    let map_if_some ~default f = function None -> default | Some v -> f v in
    let readable , writeable, _exceptfds =
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
              (if List.exists (fun fd -> equal fd req.io) set then cb else None) in
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
      let()= assert(List.compare_lengths errors requests < 1) in
      let() =assert(List.compare_lengths new_states requests < 1) in
      errors,
      List.rev_map (fun old ->
          (* update with new state if applicable: *)
          try List.find (fun n -> equal n.io old.io) new_states
          with Not_found -> old
        ) requests
      |> List.partition (fun r -> r.read_cb = None && r.write_cb = None)
    in
    let completed, failed = List.partition (fun r -> r.state = Done) inactive in
    debug (fun m ->
        m"errs: %d initial reqs: %d live requests: %d \
          completed: %d  failed: %d\n%!"
          (List.length errors) (List.length requests)
          (List.length live_requests)
          (List.length completed) (List.length failed)) ;
    List.iter (fun err -> debug (fun m ->m"request error: %s\n%!" err)) errors ;
    (* TODO expose errors in return code *)
    if [] <> live_requests
    then (select_loop[@tailcall]) ~requests:live_requests
    else (if [] = errors then Ok (debug (fun m -> m"Finishing main loop\n"))
          else Error "exiting with errors")
  in
  debug (fun m -> m"MAIN LOOP WITH %d requests\n%!" @@ List.length requests) ;
  select_loop ~requests

let urlparse orig_str =
  let parse_port str off len =
    begin match int_of_string String.(sub str off len) with
      | exception (Failure _) -> Error "urlparse: invalid port"
      | port when port < 0 || 0xFFFF < port ->
        Error "urlparse: invalid port range"
      | port -> Ok port
    end
  in
  (* NOTE this is by no means a full implementation of RFC 2396 *)
  let url = String.split_on_char '#' orig_str |> List.hd in
  begin match substr_equal ~off:0 (Bytes.of_string url) "https://",
              substr_equal ~off:0 (Bytes.of_string url) "http://" with
  | true, _->  Ok (8 (* "https://" *), "tls", 443)
  | _, true -> Ok (7 (* "http://"  *), "tcp", 80)
  | false, false -> Error "unknown protocol in URL"
  end
  >>= fun (protocol_offset, protocol, default_port) ->
  res_assert "empty URL" (protocol_offset < String.length url)
  >>= fun () ->
  let host_port_uri =
    let s = ( String.sub url protocol_offset
                (String.length url - protocol_offset) ) in
    if not (String.contains s '/') then String.concat "/" [s;""] else s in
  (* carve out hostname and port:*)
  begin
    match String.index host_port_uri '/' with
  | exception Not_found -> Error "no / in URL"
  | first_slash when host_port_uri.[0] = '[' ->
    (* RFC 2732-style IPv6 address:*)
    begin match String.index_from host_port_uri 1 ']' with
      | exception Not_found -> Error "No end-brace ']' in IPv6 URL"
      | x when x > first_slash -> Error "Invalid IPv6 URL, contains / inside []"
      | v6_end ->
        let host = String.sub host_port_uri (0+1) (v6_end-(0+1)) in
        (if first_slash - v6_end <= 1 (* empty or just colon *)
         then Ok default_port
         else if host_port_uri.[v6_end+1] = ':'
         then
           parse_port host_port_uri (v6_end+2) (first_slash-v6_end-2)
         else Error "parsing port"
        ) >>| fun port -> first_slash, host, port
    end
  | first_slash ->
    begin match String.(sub host_port_uri 0 first_slash
                        |> split_on_char ':') with
      | host::port::[] ->
        parse_port port 0 String.(length port) >>|
        fun port -> first_slash, host, port
      | [host] -> Ok (first_slash, host, default_port)
      | _ -> Error ("wtf is this host:port tuple" ^ host_port_uri)
    end
  end
  >>= fun (slash, host, port) ->
  if host = "" then Error "empty hostname"
      else Ok
          (protocol, host, port,
           String.sub host_port_uri slash (String.length host_port_uri - slash))
