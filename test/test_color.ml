open Tmx
open Color

let check_raises_invalid_arg f v =
  let exn = Error.Error (`Invalid_arg ("color", v)) in
  Alcotest.check_raises "exn" exn @@ fun () -> ignore (f v)

let check_string_conv f s s_exp =
  Alcotest.(check string) "=" s_exp (f (of_string s))

let tc_argb =
  Alcotest.test_case "argb produces correct color" `Quick @@ fun () ->
  let t = argb (-0x01) 0x00 0xff 0x100 in
  Alcotest.(check int) "=" 0x00 (a t) ;
  Alcotest.(check int) "=" 0x00 (r t) ;
  Alcotest.(check int) "=" 0xff (g t) ;
  Alcotest.(check int) "=" 0xff (b t)

let tc_rgb =
  Alcotest.test_case "rgb produces correct color" `Quick @@ fun () ->
  let t = rgb (-0x01) 0x10 0x100 in
  Alcotest.(check int) "=" 0xff (a t) ;
  Alcotest.(check int) "=" 0x00 (r t) ;
  Alcotest.(check int) "=" 0x10 (g t) ;
  Alcotest.(check int) "=" 0xff (b t)

let t_constructors = ("Constructors", [tc_argb; tc_rgb])

let tc_of_string_argb =
  Alcotest.test_case "Convert from ARGB string" `Quick @@ fun () ->
  let t_exp = argb 0xab 0xcd 0x12 0x34 in
  Alcotest.check (module Color) "=" t_exp (of_string_argb "abcd1234") ;
  Alcotest.check (module Color) "=" t_exp (of_string_argb "#abcd1234") ;
  check_raises_invalid_arg of_string_argb "#abcd123" ;
  check_raises_invalid_arg of_string_argb "abcd12345"

let tc_of_string_rgb =
  Alcotest.test_case "Convert from RGB string" `Quick @@ fun () ->
  let t_exp = argb 0xff 0xcd 0x12 0x34 in
  Alcotest.check (module Color) "=" t_exp (of_string_rgb "cd1234") ;
  Alcotest.check (module Color) "=" t_exp (of_string_rgb "#cd1234") ;
  check_raises_invalid_arg of_string_rgb "#abcd1" ;
  check_raises_invalid_arg of_string_rgb "abcd123"

let tc_of_string =
  Alcotest.test_case "Convert from ARGB or RGB string" `Quick @@ fun () ->
  let t_exp = argb 0xab 0xcd 0x12 0x34 in
  Alcotest.check (module Color) "=" t_exp (of_string "abcd1234") ;
  Alcotest.check (module Color) "=" t_exp (of_string "#abcd1234") ;
  let t_exp = argb 0xff 0xcd 0x12 0x34 in
  Alcotest.check (module Color) "=" t_exp (of_string "cd1234") ;
  Alcotest.check (module Color) "=" t_exp (of_string "#cd1234") ;
  check_raises_invalid_arg of_string "#abcd123" ;
  check_raises_invalid_arg of_string "abcd12345" ;
  check_raises_invalid_arg of_string "#abcd1" ;
  check_raises_invalid_arg of_string "abcd123"

let tc_to_string_argb =
  Alcotest.test_case "Convert to ARGB string" `Quick @@ fun () ->
  check_string_conv to_string_argb "#0a0b0304" "#0a0b0304" ;
  check_string_conv to_string_argb "#0b0304" "#ff0b0304"

let tc_to_string_rgb =
  Alcotest.test_case "Convert to RGB string" `Quick @@ fun () ->
  check_string_conv to_string_rgb "#0b0304" "#0b0304" ;
  check_string_conv to_string_rgb "#0a0b0304" "#0b0304"

let tc_to_string =
  Alcotest.test_case "Convert to ARGB or RGB string" `Quick @@ fun () ->
  check_string_conv to_string "#0a0b0304" "#0a0b0304" ;
  check_string_conv to_string "#0b0304" "#0b0304"

let t_conversions =
  ( "Conversions",
    [ tc_of_string_argb; tc_of_string_rgb; tc_of_string; tc_to_string_argb;
      tc_to_string_rgb; tc_to_string ] )

let () = Alcotest.run "Color" [t_constructors; t_conversions]
