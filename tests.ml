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

let tests =
  [ "parse_content_range", `Quick, test_parse_content_range
  ]

let qcheck_parse_content_range =
  QCheck.Test.make ~count:30000
    ~name:"quickcheck_parse_content_range"
    QCheck.(triple string string string)
    (fun (str1,str2,str3) ->
       ignore @@ parse_content_range str1 ;
       ignore @@ parse_content_range @@ "bytes " ^ str1 ^ "-" ^ str2 ;
       ignore @@ parse_content_range @@ "bytes " ^ str1 ^ "-" ^ str2 ^"/" ^ str3
       ; true
    )

let qcheck_urlparse =
  QCheck.Test.make ~count:30000
    ~name:"quickcheck_urlparse"
    QCheck.(pair string small_int)
    (fun (str, port) ->
       (* hopefully we don't throw exceptions: *)
       (ignore @@ urlparse ("http://"^str); true) &&
       (ignore @@ urlparse str ; true) &&
       (ignore @@ urlparse ("http://example.com" ^ str) ; true) &&
       (* domains: *)
       (match urlparse ("http://example.com/" ^ str) with
        | Ok ("http", "example.com", 80, _) -> true
        | _ -> false) &&
       (* IPv6: *)
       (match urlparse ("http://[2600::aaaa]/" ^ str) with
        | Ok ("http", "2600::aaaa", _, _) -> true
        | _ -> false) &&
       (match urlparse ("http://[2600::aaaa]:81/x" ^ str) with
        | Ok ("http", "2600::aaaa", 81, _) -> true
        | _ -> false) &&
       (match urlparse ("http://[2600::aaaa]:"^(string_of_int port)
                        ^ "/" ^ str) with
       | Ok ("http", "2600::aaaa", p_port, _) when p_port = port -> true
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
       | Ok ("http", "example.com", parsed_port, _)
         when port = parsed_port -> true
       | _ -> false)
    )

let qcheck_substr_equal_exn =
  QCheck.Test.make ~count:30000
    ~name:"quickcheck_substr_equal"
    QCheck.(quad int string int string)
    (fun (off, haystack, len, needle) ->
       ignore @@ substr_equal ~off (Bytes.unsafe_of_string haystack)
         ~len needle ;
       true
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
    qcheck_chunked_length
  ]

let () =
  Alcotest.run ~and_exit:false "ofetch test suite" ["Ofetch.", tests] ;
  ignore @@ QCheck_runner.run_tests ~colors:true qcheck_tests
