open Ofetch;;
(*TODO use alcotest + qcheck *)
let () =
  (* test vectors for {Content-Range: bytes}, from the RFC 7233: *)
  let parse_range str strct =
    Ofetch.parse_content_range str = Ok strct
  in
  (* https://tools.ietf.org/html/rfc7233#section-4.2 *)
  assert( parse_range "bytes 42-1233/1234" ((Some 42, Some 1233), Some 1234) );
  assert( parse_range "bytes 42-1233/*" ((Some 42, Some 1233), None) );
  assert( parse_range "bytes */1234" ((None, None), Some 1234) );
  (* - first 500 bytes: *)
  assert( parse_range "bytes 0-499/1234" ((Some 0, Some 499), Some 1234) );
  (* - second 500 bytes: *)
  assert( parse_range "bytes 500-999/1234" ((Some 500, Some 999), Some 1234) );
  (* - all except first 500 bytes: *)
  assert( parse_range "bytes 500-1233/1234" ((Some 500,Some 1233), Some 1234) );
  (* - last 500 bytes: *)
  assert( parse_range "bytes 734-1233/1234" ((Some 734, Some 1233), Some 1234)
        );

  (* - from example of If-Range: *)
  assert(
    parse_range "bytes 21010-47021/47022" ((Some 21010, Some 47021), Some 47022)
  );

  let is_error = function Ok _ -> false | Error _ -> true in
  assert( is_error @@ parse_content_range "bytes 1-3/3" );
  assert( is_error @@ parse_content_range "bytes 3-3/3" );
  assert( not @@ is_error @@ parse_content_range "bytes 2-2/3" );

  (* ""A client that is requesting multiple ranges SHOULD list those ranges
   in ascending order (the order in which they would typically be
   received in a complete representation) unless there is a specific
   need to request a later part earlier.""
  *) (*NB: ofetc doesn't implement multiple ranges per request/response.*)
