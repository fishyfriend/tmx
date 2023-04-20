open Tmx__

let loader = Loader.make ~root:(Sys.getcwd ())

module Loader = (val loader)

let tc_single1 =
  Alcotest.test_case "single1.tsx" `Quick @@ fun () ->
  let t = Loader.load_tileset_xml_exn "data/single1.tsx" in
  ignore t

let tc_coll1 =
  Alcotest.test_case "coll1.tsx" `Quick @@ fun () ->
  let t = Loader.load_tileset_xml_exn "data/coll1.tsx" in
  ignore t

let test_all = ("All", [tc_single1; tc_coll1])

let () = Alcotest.run "Tileset" [test_all]
