open Tmx
open Gid

let check_consistent flags id =
  let t = make ~flags id in
  Alcotest.check (module Flags) "=" flags (Gid.flags t) ;
  Alcotest.(check int) "=" id (Gid.id t)

let check_id_rejected id =
  let exn = Error.Error (`Invalid_arg ("id", string_of_int id)) in
  Alcotest.check_raises "exn" exn @@ fun () -> ignore (make id)

let tc_bits =
  Alcotest.test_case "Flags and ID are independent" `Quick @@ fun () ->
  check_consistent Flags.all 0 ;
  check_consistent Flags.all max_id ;
  check_consistent Flags.empty 0 ;
  check_consistent Flags.empty max_id

let tc_bounds =
  Alcotest.test_case "Enforce ID bounds" `Quick @@ fun () ->
  check_id_rejected (-1) ;
  check_id_rejected (max_id + 1) ;
  ignore (make 0) ;
  ignore (make max_id)

let t_all = ("All", [tc_bits; tc_bounds])

let () = Alcotest.run "Gid" [t_all]
