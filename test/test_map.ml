open Tmx

let loader = Loader.make ~root:(Sys.getcwd ())

module Loader = (val loader)

let tc_fixed1 =
  Alcotest.test_case "fixed1.tmx" `Quick @@ fun () ->
  let t = Loader.load_map_xml_exn "data/fixed1.tmx" in
  ignore t

let test_all = ("All", [tc_fixed1])

let () = Alcotest.run "Map" [test_all]
