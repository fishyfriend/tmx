open Tmx

let check_consistent flags id =
  let t = Gid.make ~flags id in
  Alcotest.check (module Gid.Flags) "=" flags (Gid.flags t) ;
  Alcotest.(check int) "=" id (Gid.id t)

let check_id_rejected id =
  let exn = Error.Error (`Invalid_arg ("id", string_of_int id)) in
  Alcotest.check_raises "exn" exn @@ fun () -> ignore (Gid.make id)

let tc_bits =
  Alcotest.test_case "Flags and ID are independent" `Quick @@ fun () ->
  check_consistent Gid.Flags.all 0 ;
  check_consistent Gid.Flags.all Gid.max_id ;
  check_consistent Gid.Flags.empty 0 ;
  check_consistent Gid.Flags.empty Gid.max_id

let tc_bounds =
  Alcotest.test_case "Enforce ID bounds" `Quick @@ fun () ->
  check_id_rejected (-1) ;
  check_id_rejected (Gid.max_id + 1) ;
  ignore (Gid.make 0) ;
  ignore Gid.(make max_id)

let t_all = ("All", [tc_bits; tc_bounds])

let () = Alcotest.run "Gid" [t_all]
