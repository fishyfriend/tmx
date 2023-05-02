module A = Alcotest

open Tmx__

module Loader = (val Loader.make ~root:(Sys.getcwd ()) ())

open Loader
open Helpers.Make (Core)

module type PropsT = Sigs.PropsT with type property := Property.t

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
  A.check (module Gid) "equal" gid (Tilelayer.gid_at ~col ~row tl)

let check_tile_at ~tl ~col ~row ~ts ~tile =
  let ts = get_tileset_exn ts in
  let tile_exp = Tileset.get_tile_exn ts tile in
  let gid = Tilelayer.gid_at ~col ~row tl in
  let tile = get_tile_exn gid in
  A.check (module Tile) "equal" tile_exp tile

let check_tileset_image_file ~ts fname_exp =
  get_tileset_exn ts |> Tileset.variant
  |> (function `Single s -> s | _ -> assert false)
  |> Tileset.Single.image |> Image.source
  |> (function `File fname -> fname | _ -> assert false)
  |> A.(check string) "equal" fname_exp

let m = load_map_xml_exn "data/fixed1.tmx"
let tsx_c1 = "data/coll1.tsx"
let tsx_c1' = "data/subdir/alt-coll1.tsx"
let tsx_s1 = "data/single1.tsx"
let tsx_s1' = "data/subdir/alt-single1.tsx"

let tc_rsc_map =
  A.test_case "For map" `Quick @@ fun () ->
  A.(check int) "equal" 4 (List.length (tilesets ())) ;
  A.(check int) "equal" 1 (List.length (templates ()))

let tc_rsc_file =
  A.test_case "For file properties" `Quick @@ fun () ->
  A.(check string) "equal" "hello world!\n" (get_file_exn "data/hello.txt")

let test_resources = ("Load supporting resources", [tc_rsc_map; tc_rsc_file])

let tc_reloc_map =
  A.test_case "Remap paths in map" `Quick @@ fun () ->
  let tss_exp = [(1, tsx_s1); (9, tsx_c1); (12, tsx_s1'); (20, tsx_c1')] in
  A.(check (slist (pair int string) compare)) "equal" tss_exp (Map.tilesets m) ;
  let o = Map.get_object_exn m 35 in
  check_prop (module Object) o "foo" (`File "test_loader.ml") ;
  check_prop (module Map) m "bar" (`File "test_loader.ml")

let tc_reloc_tileset =
  A.test_case "Remap paths in tileset" `Quick @@ fun () ->
  check_tileset_image_file ~ts:tsx_s1 "data/tileset.png" ;
  check_tileset_image_file ~ts:tsx_s1' "data/tileset.png" ;
  let ts = get_tileset_exn tsx_s1 in
  check_prop (module Tileset) ts "quux" (`File "data/hello.txt")

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
  A.test_case "Remap tile data GIDs" `Quick @@ fun () ->
  let tl =
    Map.get_layer_exn m 1 |> Layer.variant |> function
    | `Tilelayer tl -> tl
    | _ -> assert false in
  check_gid_at ~tl ~col:0 ~row:0 (Gid.make 0) ;
  check_tile_at ~tl ~col:1 ~row:0 ~ts:tsx_s1 ~tile:0 ;
  check_tile_at ~tl ~col:8 ~row:0 ~ts:tsx_s1 ~tile:7 ;
  check_tile_at ~tl ~col:9 ~row:0 ~ts:tsx_s1' ~tile:0 ;
  check_tile_at ~tl ~col:16 ~row:0 ~ts:tsx_s1' ~tile:7

let test_gids = ("Remap GIDs", [tc_gids_object; tc_gids_tile])

let () = A.run "Loader" [test_resources; test_reloc; test_gids]
