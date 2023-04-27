open Tmx__
open Core.Simple

let check_int t o v =
  Alcotest.(check (option int)) "equal" o (Enum.read_as_int t v)

let check_string t o v =
  Alcotest.(check (option string)) "equal" o (Enum.read_as_string t v)

let check_alist t o v =
  Alcotest.(check (option (list (pair string bool))))
    "equal" o (Enum.read_as_alist t v)

let tc_int_flags =
  Alcotest.test_case "Int flags" `Quick @@ fun () ->
  let t = Enum.make ~storagetype:`Int ~valuesasflags:true ["1"; "2"; "4"] in
  check_int t (Some 0) (`Int 0) ;
  check_int t (Some 1) (`Int 1) ;
  check_int t (Some 7) (`Int 7) ;
  check_int t (Some 7) (`Int 15) ;
  check_string t (Some "") (`Int 0) ;
  check_string t (Some "1") (`Int 1) ;
  check_string t (Some "1,2,4") (`Int 7) ;
  check_string t (Some "1,2,4") (`Int 15) ;
  check_alist t (Some [("1", false); ("2", false); ("4", false)]) (`Int 0) ;
  check_alist t (Some [("1", true); ("2", false); ("4", false)]) (`Int 1) ;
  check_alist t (Some [("1", true); ("2", true); ("4", true)]) (`Int 7) ;
  check_alist t (Some [("1", true); ("2", true); ("4", true)]) (`Int 15)

let tc_int_nonflags =
  Alcotest.test_case "Int non-flags" `Quick @@ fun () ->
  let t = Enum.make ~storagetype:`Int ~valuesasflags:false ["0"; "1"; "2"] in
  check_int t (Some 0) (`Int 0) ;
  check_int t (Some 1) (`Int 1) ;
  check_int t (Some 2) (`Int 2) ;
  check_int t None (`Int 3) ;
  check_string t (Some "0") (`Int 0) ;
  check_string t (Some "1") (`Int 1) ;
  check_string t (Some "2") (`Int 2) ;
  check_string t None (`Int 3) ;
  check_alist t (Some [("0", true); ("1", false); ("2", false)]) (`Int 0) ;
  check_alist t (Some [("0", false); ("1", true); ("2", false)]) (`Int 1) ;
  check_alist t (Some [("0", false); ("1", false); ("2", true)]) (`Int 2) ;
  check_alist t None (`Int 3)

let tc_string_flags =
  Alcotest.test_case "String flags" `Quick @@ fun () ->
  let t = Enum.make ~storagetype:`String ~valuesasflags:true ["1"; "2"; "4"] in
  check_int t (Some 0) (`String "0") ;
  check_int t (Some 1) (`String "1") ;
  check_int t (Some 7) (`String "2,1,4") ;
  check_int t (Some 7) (`String "1,2,4,x") ;
  check_string t (Some "") (`String "0") ;
  check_string t (Some "1") (`String "1") ;
  check_string t (Some "1,2,4") (`String "2,1,4") ;
  check_string t (Some "1,2,4") (`String "2,1,4,x") ;
  check_alist t (Some [("1", false); ("2", false); ("4", false)]) (`String "") ;
  check_alist t (Some [("1", true); ("2", false); ("4", false)]) (`String "1") ;
  check_alist t (Some [("1", true); ("2", true); ("4", true)]) (`String "2,1,4") ;
  check_alist t
    (Some [("1", true); ("2", true); ("4", true)])
    (`String "2,1,4,x")

let tc_string_nonflags =
  Alcotest.test_case "String non-flags" `Quick @@ fun () ->
  let t = Enum.make ~storagetype:`String ~valuesasflags:false ["0"; "1"; "2"] in
  check_int t (Some 0) (`String "0") ;
  check_int t (Some 2) (`String "2") ;
  check_int t None (`String "3") ;
  check_int t None (`String "0,1") ;
  check_string t (Some "0") (`String "0") ;
  check_string t (Some "2") (`String "2") ;
  check_string t None (`String "3") ;
  check_string t None (`String "0,1") ;
  check_alist t (Some [("0", true); ("1", false); ("2", false)]) (`String "0") ;
  check_alist t (Some [("0", false); ("1", false); ("2", true)]) (`String "2") ;
  check_alist t None (`String "3") ;
  check_alist t None (`String "0,1")

let t_all =
  ("All", [tc_int_flags; tc_int_nonflags; tc_string_flags; tc_string_nonflags])

let () = Alcotest.run "Enum" [t_all]
