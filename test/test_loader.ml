open Tmx__

module A = Alcotest
module L = (val Loader.make ~root:"." ())
module H = Helpers.Make (L.Core)

module type PropsT = Sigs.PropsT with type property := L.Property.t

let check_object_tile ~m ~o ~ts ~tile =
  let ts = L.get_tileset_exn ts in
  let tile_exp = L.Tileset.get_tile_exn ts tile in
  let o = L.Map.get_object_exn m o in
  let tile =
    match L.Object.shape o with
    | `Tile gid -> L.get_tile_exn gid
    | _ -> assert false in
  A.(check (module L.Tile)) "equal" tile_exp tile

let check_gid_at ~tl ~col ~row gid =
  A.check (module L.Gid) "equal" gid (L.Tilelayer.gid_at ~col ~row tl)

let check_tile_at ~tl ~col ~row ~ts ~tile =
  let ts = L.get_tileset_exn ts in
  let tile_exp = L.Tileset.get_tile_exn ts tile in
  let gid = L.Tilelayer.gid_at ~col ~row tl in
  let tile = L.get_tile_exn gid in
  A.check (module L.Tile) "equal" tile_exp tile

let check_tileset_image_file ~ts fname_exp =
  L.get_tileset_exn ts |> L.Tileset.variant
  |> (function `Single s -> s | _ -> assert false)
  |> L.Tileset.Single.image |> L.Image.source
  |> (function `File fname -> fname | _ -> assert false)
  |> A.(check string) "equal" fname_exp

let m = L.load_map_xml_exn "data/fixed1.tmx"
let tsx_c1 = "data/coll1.tsx"
let tsx_c1' = "data/subdir/alt-coll1.tsx"
let tsx_s1 = "data/single1.tsx"
let tsx_s1' = "data/subdir/alt-single1.tsx"

let tc_rsc_map =
  A.test_case "Load map deps" `Quick @@ fun () ->
  A.(check int) "equal" 4 (List.length (L.tilesets ())) ;
  A.(check int) "equal" 1 (List.length (L.templates ()))

let tc_rsc_miss_file =
  A.test_case "Check prop files" `Quick @@ fun () ->
  let f = "data/bad-file.tsx" in
  let e =
    let f = Filename.concat (Sys.getcwd ()) "data/confession.txt" in
    Error.Error (`Not_found ("file", f)) in
  let module L1 = (val Loader.make ~root:"." ~property_files:`Load ()) in
  A.check_raises "raises" e (fun () -> ignore (L1.load_tileset_xml_exn f)) ;
  let module L2 = (val Loader.make ~root:"." ~property_files:`Check ()) in
  A.check_raises "raises" e (fun () -> ignore (L2.load_tileset_xml_exn f)) ;
  let module L3 = (val Loader.make ~root:"." ~property_files:`Ignore ()) in
  ignore (L3.load_tileset_xml_exn f)

let tc_rsc_file =
  A.test_case "Load prop files" `Quick @@ fun () ->
  A.(check string) "equal" "hello world!\n" (L.get_file_exn "data/hello.txt")

let tc_rsc_image =
  A.test_case "Check images" `Quick @@ fun () ->
  let f = "data/bad-image.tsx" in
  let e =
    let f = Filename.concat (Sys.getcwd ()) "data/ghost.png" in
    Error.Error (`Not_found ("file", f)) in
  let module L1 = (val Loader.make ~root:"." ~image_files:`Check ()) in
  A.check_raises "raises" e (fun () -> ignore (L1.load_tileset_xml_exn f)) ;
  let module L2 = (val Loader.make ~root:"." ~image_files:`Ignore ()) in
  ignore (L2.load_tileset_xml_exn f)

let test_resources =
  ( "Supporting resources",
    [tc_rsc_map; tc_rsc_miss_file; tc_rsc_file; tc_rsc_image] )

let tc_reloc_map =
  A.test_case "Relocate map" `Quick @@ fun () ->
  let tss_exp = [(1, tsx_s1); (9, tsx_c1); (12, tsx_s1'); (20, tsx_c1')] in
  A.(check (slist (pair int string) compare))
    "equal" tss_exp (L.Map.tilesets m) ;
  let o = L.Map.get_object_exn m 35 in
  H.check_prop (module L.Object) o "foo" (`File "test_loader.ml") ;
  H.check_prop (module L.Map) m "bar" (`File "test_loader.ml")

let tc_reloc_tileset =
  A.test_case "Relocate tileset" `Quick @@ fun () ->
  check_tileset_image_file ~ts:tsx_s1 "data/tileset.png" ;
  check_tileset_image_file ~ts:tsx_s1' "data/tileset.png" ;
  let ts = L.get_tileset_exn tsx_s1 in
  H.check_prop (module L.Tileset) ts "quux" (`File "data/hello.txt")

let test_reloc = ("Relocate paths", [tc_reloc_map; tc_reloc_tileset])

let tc_gids_object =
  A.test_case "Remap object GIDs" `Quick @@ fun () ->
  check_object_tile ~m ~o:3 ~ts:tsx_c1 ~tile:0 ;
  check_object_tile ~m ~o:7 ~ts:tsx_c1 ~tile:1 ;
  check_object_tile ~m ~o:18 ~ts:tsx_c1 ~tile:2 ;
  check_object_tile ~m ~o:34 ~ts:tsx_c1' ~tile:0 ;
  check_object_tile ~m ~o:35 ~ts:tsx_c1' ~tile:1 ;
  check_object_tile ~m ~o:37 ~ts:tsx_c1' ~tile:2

let tc_gids_tile =
  A.test_case "Remap tilemap GIDs" `Quick @@ fun () ->
  let tl =
    L.Map.get_layer_exn m 1 |> L.Layer.variant |> function
    | `Tilelayer tl -> tl
    | _ -> assert false in
  check_gid_at ~tl ~col:0 ~row:0 (L.Gid.make 0) ;
  check_tile_at ~tl ~col:1 ~row:0 ~ts:tsx_s1 ~tile:0 ;
  check_tile_at ~tl ~col:8 ~row:0 ~ts:tsx_s1 ~tile:7 ;
  check_tile_at ~tl ~col:9 ~row:0 ~ts:tsx_s1' ~tile:0 ;
  check_tile_at ~tl ~col:16 ~row:0 ~ts:tsx_s1' ~tile:7

let test_gids = ("Remap GIDs", [tc_gids_object; tc_gids_tile])

let () = A.run "Loader" [test_resources; test_reloc; test_gids]
