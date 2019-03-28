open Ofetch

let test_parse_content_range () : unit =
  (* test vectors for {Content-Range: bytes}, from the RFC 7233: *)
  let range_testable = Alcotest.testable Ofetch.pp_content_range
     (fun a b -> 0 = compare a b) in
  let cr first last complete =
    Ok ( Content_range { first ; last; complete } )
  in
  (* https://tools.ietf.org/html/rfc7233#section-4.2 *)
  Alcotest.(check (result range_testable reject)) "bytes 42-1233/1234"
    (parse_content_range "bytes 42-1233/1234") (cr 42_L 1233_L (Some 1234_L)) ;
  Alcotest.(check (result range_testable reject)) "bytes 42-1233/*"
    (parse_content_range "bytes 42-1233/*") (cr 42_L 1233_L None) ;
  Alcotest.(check (result range_testable reject)) "bytes */1234"
    (parse_content_range "bytes */1234") (Ok (Unsatisfiable_range 1234_L)) ;
  Alcotest.(check (result range_testable reject)) "first 500 bytes"
    (parse_content_range "bytes 0-499/1234") (cr 0_L 499_L (Some 1234_L)) ;
  (* - second 500 bytes: *)
  Alcotest.(check (result range_testable reject)) "second 500 bytes"
    (parse_content_range "bytes 500-999/1234") (cr 500_L 999_L (Some 1234_L)) ;
  (* - all except first 500 bytes: *)
  Alcotest.(check (result range_testable reject)) "all except first 500 bytes"
    (parse_content_range "bytes 500-1233/1234") (cr 500_L 1233_L (Some 1234_L));
  (* - last 500 bytes: *)
  Alcotest.(check (result range_testable reject)) "last 500 bytes"
    (parse_content_range "bytes 734-1233/1234") (cr 734_L 1233_L (Some 1234_L));

  (* - from example of If-Range: *)
  Alcotest.(check (result range_testable reject)) "from example of If-Range"
    (parse_content_range "bytes 21010-47021/47022")
    (cr 21010_L 47021_L (Some 47022_L)) ;

  Alcotest.(check (result range_testable reject)) "pass first = last"
    (parse_content_range "bytes 2-2/3") (cr 2_L 2_L (Some 3_L)) ;

    Alcotest.(check (result reject pass)) "fail unsatisfiable negative complete"
    (parse_content_range "bytes */-1") (Error "") ;
  Alcotest.(check (result reject pass)) "fail complete on overflow"
    (parse_content_range "bytes 0-2/0xffffffffffffffff") (Error "") ;
  Alcotest.(check (result reject pass)) "fail last = complete"
    (parse_content_range "bytes 1-3/3") (Error "") ;
  Alcotest.(check (result reject pass)) "fail last > complete"
    (parse_content_range "bytes 1-4/3") (Error "") ;
  Alcotest.(check (result reject pass)) "fail first > last"
    (parse_content_range "bytes 3-2/4") (Error "") ;
  Alcotest.(check (result reject pass)) "fail first > complete"
    (parse_content_range "bytes 4-2/3") (Error "") ;
  ()

  (* ""A client that is requesting multiple ranges SHOULD list those ranges
   in ascending order (the order in which they would typically be
   received in a complete representation) unless there is a specific
   need to request a later part earlier.""
  *) (*NB: ofetch doesn't implement multiple ranges per request/response.*)

let test_header_parsing () =
  (* ensures that reads of exactly buffer size are accepted, and that we
     finish parsing the headers .*)
  let mock_response, expected_body_size =
    let headers = "200 OK\r\nHost: yo\r\nMock: x\r\n\r\n" in
    let body = String.(make (8192 - 4- length headers)) 'a' in
    headers ^ body ^ "\r\n\r\n", String.length body in
  let written_body_size = ref 0 in
  let write_local _ _ len = written_body_size := !written_body_size + len ; len in
  let recv_peer buf off len =
    let actual = min len @@ String.length mock_response in
    Bytes.blit_string mock_response off buf off actual ;
    Ok actual in
  let write_peer _buf _off len = len in
  Printexc.record_backtrace true;
  Ofetch.ofetch_global_debugging := true;
  let rec loop counter state =
    match Ofetch.fetch_download ~write_local ~recv_peer state with
    | Ok ({ state = Done ; _}, None) -> ()
    | Ok (req, None) when counter < 10000 -> loop (succ counter) req
    | Ok _ -> failwith "too many loops or request rescheduled"
    | Error s -> failwith s ;
  in
  let req : unit Ofetch.request =
    Ofetch.new_request ~connection_handle:() ~buflen:8192
      ~write_local ~recv_peer ~write_peer ~path:"/" ~hostname:"yo" in
  loop 1 req ;
  assert (!written_body_size = expected_body_size) ;
  Ofetch.ofetch_global_debugging := false

let qcheck_parse_headers =
(*  Printexc.record_backtrace true;*)
  QCheck.Test.make ~count:10000
    ~name:"quickcheck_parse_headers"
    QCheck.(quad (int_range 0 16400) (int_range 1 3000) (int_range 1 3000) (int_range 128 16384))
    (fun (a_size, read_size, write_size, buflen) ->
       let mock_response, expected_body_size =
         let headers = "200 OK\r\nHost: yo\r\nMock: x\r\n\r\ne" in
         let body = String.(make (a_size)) 'a' in
         headers ^ body ^ "o\r\n\r\n", String.length body+2 in
       let written_body_size = ref 0 in
       let consumed_bytes = ref 0 in
       let write_local _buf _off len =
         (*Printf.printf "writing local: %d: %S\n%!" len (Bytes.sub_string buf off len);*)
         written_body_size := !written_body_size + len ; len in
       let recv_peer buf off len =
         let actual = min len @@
           (String.length mock_response - !consumed_bytes)
                    |> min read_size in
         Bytes.blit_string mock_response (!consumed_bytes) buf off actual ;
         consumed_bytes := !consumed_bytes + actual ;
         Ok actual in
       let write_peer _buf _off len = min len write_size in
       let rec loop counter state =
         Ofetch.ofetch_global_debugging := false ;
         match Ofetch.fetch_download ~write_local ~recv_peer state with
         | Ok (_, Some _) -> failwith "FUCK CALLBACK"
         | Ok ({ state = Done ; saved_bytes ; _ }, None) ->
           if !written_body_size = expected_body_size then true
           else (
             Printf.printf "%d <> %d <> %d <> !saved:%d\n"
               !written_body_size expected_body_size a_size !saved_bytes;
             false )
         | Ok (req, None) when counter < 30000 ->
           loop (succ counter) req
         | Ok _ when counter = 30000 ->
           Printf.printf "looped 30k times fuck this\n%!"; false
         | Ok _ ->
           Printf.printf "OK SOMETHING ELSE\n%!"; ignore@@failwith "other ok";false
         | Error s -> Printf.printf "ERROR: %S\n%!" s ; failwith s
       in
       (*Printf.printf "--new REQUEST-- %d\n%!" a_size;*)
       let req : unit Ofetch.request =
         Ofetch.new_request ~connection_handle:() ~buflen
           ~write_local ~recv_peer ~write_peer ~path:"/" ~hostname:"yo" in
       loop 1 req
    ) |> fun x ->
  Ofetch.ofetch_global_debugging := false ; x

let tests =
  [ "parse_content_range", `Quick, test_parse_content_range ;
    "header parsing", `Quick, test_header_parsing ;
  ]

let qcheck_parse_content_range =
  QCheck.Test.make ~count:3000
    ~name:"quickcheck_parse_content_range"
    QCheck.(triple string string string)
    (fun (str1,str2,str3) ->
       ignore @@ parse_content_range str1 ;
       ignore @@ parse_content_range @@ "bytes " ^ str1 ^ "-" ^ str2 ;
       ignore @@ parse_content_range @@ "bytes " ^ str1 ^ "-" ^ str2 ^"/" ^ str3
       ; true
    )

let qcheck_urlparse =
  QCheck.Test.make ~count:3000
    ~name:"quickcheck_urlparse"
    QCheck.(pair string small_int)
    (fun (str, port) ->
       (* hopefully we don't throw exceptions: *)
       (ignore @@ urlparse ("http://"^str); true) &&
       (ignore @@ urlparse str ; true) &&
       (ignore @@ urlparse ("http://example.com" ^ str) ; true) &&
       (* domains: *)
       (match urlparse ("http://example.com/" ^ str) with
        | Ok ("tcp", "example.com", 80, _) -> true
        | _ -> false) &&
       (* IPv6: *)
       (match urlparse ("http://[2600::aaaa]/" ^ str) with
        | Ok ("tcp", "2600::aaaa", 80, _) -> true
        | _ -> false) &&
       (match urlparse ("http://[2600::aaaa]:81/x" ^ str) with
        | Ok ("tcp", "2600::aaaa", 81, _) -> true
        | _ -> false) &&
       (match urlparse ("http://[2600::aaaa]:"^(string_of_int port)
                        ^ "/" ^ str) with
       | Ok ("tcp", "2600::aaaa", p_port, _) when p_port = port -> true
       | _ -> false) &&
       (match urlparse ("http://[:/" ^ str) with
        | Error "Invalid IPv6 URL, contains / inside []" ->
          true (* <- this is invalid, so we do expect an error *)
        | Error "No end-brace ']' in IPv6 URL" when
            not (String.contains str ']')
            || ( (* urlparser drops things after '#': *)
              String.contains str '#' &&
              String.index str '#' < String.index str ']'
            ) -> true (* <- we do expect a ] after a [ *)
        | _ -> false) &&
       (* ports: *)
       (match urlparse ("http://example.com:" ^ (string_of_int port)
                        ^ "/" ^ str) with
       | Ok ("tcp", "example.com", parsed_port, _)
         when port = parsed_port -> true
       | _ -> false)
    )

let qcheck_substr_equal_exn =
  QCheck.Test.make ~count:30000
    ~name:"quickcheck_substr_equal"
    QCheck.(quad small_int small_string small_int small_string)
    (fun (off, haystack, len, needle) ->
       match substr_equal ~off (Bytes.unsafe_of_string haystack)
               ~len needle with
       | false when len < String.length needle -> true
       | false when off < 0 || off >= String.length haystack -> true
       | false when off + String.length needle > String.length haystack -> true
       | true when String.sub haystack off (min len @@ String.length needle)<> needle -> false
       | false when String.sub haystack off (String.length needle) <> needle-> true
       | x -> x
    )

let qcheck_chunked_length =
  QCheck.Test.make ~count:50000
    ~name:"quickcheck_chunked_length"
    (QCheck.int_bound 4095) (* we reject four-digit hex *)
    (fun int ->
       let c = ChunkedLength.create () in
       let x = "\r\n" ^ (Format.asprintf "%x" int) ^"\r\n" |> Bytes.of_string in
       match ChunkedLength.add c x 0 (Bytes.length x) with
       | Error _ -> false
       | Ok c2 -> ChunkedLength.to_int c2 = Ok int
    )

let qcheck_tests : QCheck.Test.t list =
  [ qcheck_parse_content_range ;
    qcheck_urlparse ;
    qcheck_substr_equal_exn ;
    qcheck_chunked_length ;
    qcheck_parse_headers ;
  ]

let () =
  Alcotest.run ~and_exit:false "ofetch test suite" ["Ofetch.", tests] ;
  exit (QCheck_runner.run_tests ~colors:true qcheck_tests)
