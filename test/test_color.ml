open Tmx

module A = Alcotest
module C = Color

let check_raises_invalid_arg f v =
  let exn = Error.Error (`Invalid_arg ("color", v)) in
  A.check_raises "exn" exn @@ fun () -> ignore (f v)

let check_string_conv f s s_exp =
  A.(check string) "=" s_exp (f (C.of_string s))

let tc_argb =
  A.test_case "argb produces correct color" `Quick @@ fun () ->
  let t = C.argb (-0x01) 0x00 0xff 0x100 in
  A.(check int) "=" 0x00 (C.a t) ;
  A.(check int) "=" 0x00 (C.r t) ;
  A.(check int) "=" 0xff (C.g t) ;
  A.(check int) "=" 0xff (C.b t)

let tc_rgb =
  A.test_case "rgb produces correct color" `Quick @@ fun () ->
  let t = C.rgb (-0x01) 0x10 0x100 in
  A.(check int) "=" 0xff (C.a t) ;
  A.(check int) "=" 0x00 (C.r t) ;
  A.(check int) "=" 0x10 (C.g t) ;
  A.(check int) "=" 0xff (C.b t)

let t_constructors = ("Constructors", [tc_argb; tc_rgb])

let tc_of_string_argb =
  A.test_case "Convert from ARGB string" `Quick @@ fun () ->
  let t_exp = C.argb 0xab 0xcd 0x12 0x34 in
  A.check (module C) "=" t_exp (C.of_string_argb "abcd1234") ;
  A.check (module C) "=" t_exp (C.of_string_argb "#abcd1234") ;
  check_raises_invalid_arg C.of_string_argb "#abcd123" ;
  check_raises_invalid_arg C.of_string_argb "abcd12345"

let tc_of_string_rgb =
  A.test_case "Convert from RGB string" `Quick @@ fun () ->
  let t_exp = C.argb 0xff 0xcd 0x12 0x34 in
  A.check (module C) "=" t_exp (C.of_string_rgb "cd1234") ;
  A.check (module C) "=" t_exp (C.of_string_rgb "#cd1234") ;
  check_raises_invalid_arg C.of_string_rgb "#abcd1" ;
  check_raises_invalid_arg C.of_string_rgb "abcd123"

let tc_of_string =
  A.test_case "Convert from ARGB or RGB string" `Quick @@ fun () ->
  let t_exp = C.argb 0xab 0xcd 0x12 0x34 in
  A.check (module C) "=" t_exp (C.of_string "abcd1234") ;
  A.check (module C) "=" t_exp (C.of_string "#abcd1234") ;
  let t_exp = C.argb 0xff 0xcd 0x12 0x34 in
  A.check (module C) "=" t_exp (C.of_string "cd1234") ;
  A.check (module C) "=" t_exp (C.of_string "#cd1234") ;
  check_raises_invalid_arg C.of_string "#abcd123" ;
  check_raises_invalid_arg C.of_string "abcd12345" ;
  check_raises_invalid_arg C.of_string "#abcd1" ;
  check_raises_invalid_arg C.of_string "abcd123"

let tc_to_string_argb =
  A.test_case "Convert to ARGB string" `Quick @@ fun () ->
  check_string_conv C.to_string_argb "#0a0b0304" "#0a0b0304" ;
  check_string_conv C.to_string_argb "#0b0304" "#ff0b0304"

let tc_to_string_rgb =
  A.test_case "Convert to RGB string" `Quick @@ fun () ->
  check_string_conv C.to_string_rgb "#0b0304" "#0b0304" ;
  check_string_conv C.to_string_rgb "#0a0b0304" "#0b0304"

let tc_to_string =
  A.test_case "Convert to ARGB or RGB string" `Quick @@ fun () ->
  check_string_conv C.to_string "#0a0b0304" "#0a0b0304" ;
  check_string_conv C.to_string "#0b0304" "#0b0304"

let t_conversions =
  ( "Conversions",
    [ tc_of_string_argb; tc_of_string_rgb; tc_of_string; tc_to_string_argb;
      tc_to_string_rgb; tc_to_string ] )

let () = A.run "Color" [t_constructors; t_conversions]
