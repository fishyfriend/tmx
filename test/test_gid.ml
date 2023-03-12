open Tmx
open Gid

let tc_bits =
  Alcotest.test_case "Flags and ID are independent" `Quick @@ fun () ->
  let check flags id =
    let t = make ~flags id in
    Alcotest.check (module Flags) "=" flags (Gid.flags t) ;
    Alcotest.(check int) "=" id (Gid.id t) in
  check Flags.all 0 ;
  check Flags.all max_id ;
  check Flags.empty 0 ;
  check Flags.empty max_id

let check_id_rejected id =
  let exn = Error.Error (`Invalid_arg ("id", string_of_int id)) in
  Alcotest.check_raises "exn" exn @@ fun () -> ignore (make id)

let tc_bounds =
  Alcotest.test_case "Enforce ID bounds" `Quick @@ fun () ->
  check_id_rejected (-1) ;
  check_id_rejected (max_id + 1) ;
  ignore (make 0) ;
  ignore (make max_id)

let t_all = ("All", [tc_bits; tc_bounds])

let () = Alcotest.run "Gid" [t_all]
