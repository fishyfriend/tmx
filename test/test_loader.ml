module A = Alcotest

open Tmx__

module Loader = (val Loader.make ~root:(Sys.getcwd ()))

open Loader

let check_object_tile ~m ~o ~ts ~tile =
  let ts = get_tileset_exn ts in
  let tile_exp = Tileset.get_tile_exn ts tile in
  let o = Map.get_object_exn m o in
  let tile =
    match Object.shape o with
    | `Tile gid -> get_tile_exn gid
    | _ -> assert false in
  A.(check (module Tile)) "equal" tile_exp tile

let check_gid_at ~tl ~col ~row gid =
  A.check (module Gid) "equal" gid (Layer.Tilelayer.gid_at ~col ~row tl)

let check_tile_at ~tl ~col ~row ~ts ~tile =
  let ts = get_tileset_exn ts in
  let tile_exp = Tileset.get_tile_exn ts tile in
  let gid = Layer.Tilelayer.gid_at ~col ~row tl in
  let tile = get_tile_exn gid in
  A.check (module Tile) "equal" tile_exp tile

let check_object_float_attr ~m ~o ~f x =
  A.(check (float 1e-3)) "equal" x (f (Map.get_object_exn m o))

let m = load_map_xml_exn "data/fixed1.tmx"
let tsx_coll1 = "data/coll1.tsx"
let tsx_alt_coll1 = "data/subdir/alt-coll1.tsx"
let tsx_single1 = "data/single1.tsx"
let tsx_alt_single1 = "data/subdir/alt-single1.tsx"

let tc_tilesets =
  A.test_case "Load tilesets for map" `Quick @@ fun () ->
  A.(check int) "equal" 4 (List.length (tilesets ()))

let tc_object_gids =
  A.test_case "Remap object GIDs" `Quick @@ fun () ->
  check_object_tile ~m ~o:3 ~ts:tsx_coll1 ~tile:0 ;
  check_object_tile ~m ~o:7 ~ts:tsx_coll1 ~tile:1 ;
  check_object_tile ~m ~o:18 ~ts:tsx_coll1 ~tile:2 ;
  check_object_tile ~m ~o:34 ~ts:tsx_alt_coll1 ~tile:0 ;
  check_object_tile ~m ~o:35 ~ts:tsx_alt_coll1 ~tile:1 ;
  check_object_tile ~m ~o:37 ~ts:tsx_alt_coll1 ~tile:2

let tc_tile_gids =
  A.test_case "Remap tile data GIDs" `Quick @@ fun () ->
  let tl =
    let l = Map.get_layer_exn m 1 in
    match Layer.variant l with `Tilelayer tl -> tl | _ -> assert false in
  check_gid_at ~tl ~col:0 ~row:0 (Gid.make 0) ;
  check_tile_at ~tl ~col:1 ~row:0 ~ts:tsx_single1 ~tile:0 ;
  check_tile_at ~tl ~col:2 ~row:0 ~ts:tsx_single1 ~tile:1 ;
  check_tile_at ~tl ~col:3 ~row:0 ~ts:tsx_single1 ~tile:2 ;
  check_tile_at ~tl ~col:4 ~row:0 ~ts:tsx_single1 ~tile:3 ;
  check_tile_at ~tl ~col:5 ~row:0 ~ts:tsx_single1 ~tile:4 ;
  check_tile_at ~tl ~col:6 ~row:0 ~ts:tsx_single1 ~tile:5 ;
  check_tile_at ~tl ~col:7 ~row:0 ~ts:tsx_single1 ~tile:6 ;
  check_tile_at ~tl ~col:8 ~row:0 ~ts:tsx_single1 ~tile:7 ;
  check_tile_at ~tl ~col:9 ~row:0 ~ts:tsx_alt_single1 ~tile:0 ;
  check_tile_at ~tl ~col:10 ~row:0 ~ts:tsx_alt_single1 ~tile:1 ;
  check_tile_at ~tl ~col:11 ~row:0 ~ts:tsx_alt_single1 ~tile:2 ;
  check_tile_at ~tl ~col:12 ~row:0 ~ts:tsx_alt_single1 ~tile:3 ;
  check_tile_at ~tl ~col:13 ~row:0 ~ts:tsx_alt_single1 ~tile:4 ;
  check_tile_at ~tl ~col:14 ~row:0 ~ts:tsx_alt_single1 ~tile:5 ;
  check_tile_at ~tl ~col:15 ~row:0 ~ts:tsx_alt_single1 ~tile:6 ;
  check_tile_at ~tl ~col:16 ~row:0 ~ts:tsx_alt_single1 ~tile:7

let tc_templates =
  A.test_case "Apply object templates" `Quick @@ fun () ->
  check_object_float_attr ~m ~o:6 ~f:Object.rotation 45. ;
  check_object_float_attr ~m ~o:6 ~f:Object.width 28. ;
  check_object_float_attr ~m ~o:6 ~f:Object.height 16. ;
  check_object_tile ~m ~o:6 ~ts:tsx_coll1 ~tile:0 ;
  check_object_float_attr ~m ~o:19 ~f:Object.rotation 45. ;
  check_object_float_attr ~m ~o:19 ~f:Object.width 56. ;
  check_object_float_attr ~m ~o:19 ~f:Object.height 32. ;
  check_object_tile ~m ~o:19 ~ts:tsx_coll1 ~tile:2

let test_all =
  ("All", [tc_tilesets; tc_object_gids; tc_tile_gids; tc_templates])

let () = A.run "Loader" [test_all]
