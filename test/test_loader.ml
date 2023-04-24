module A = Alcotest

open Tmx__

module Loader = (val Loader.make ~root:(Sys.getcwd ()))

open Loader

let o_tile o =
  match Object.shape o with `Tile gid -> get_tile_exn gid | _ -> assert false

let ts_tile ts id =
  let ts = get_tileset_exn ts in
  Option.get (Tileset.get_tile ts id)

let test_all =
  let m = load_map_xml_exn "data/fixed1.tmx" in
  let tsx_coll1 = "data/coll1.tsx" in
  let tsx_alt_coll1 = "data/subdir/alt-coll1.tsx" in
  let tsx_single1 = "data/single1.tsx" in
  let tsx_alt_single1 = "data/subdir/alt-single1.tsx" in
  let objects = List.sort Object.compare (Map.objects m) in
  let o id = List.find (fun o -> Object.id o = id) objects in

  let tc_tilesets =
    A.test_case "Tilesets" `Quick @@ fun () ->
    let tss = Map.tilesets m in
    A.(check int) "equal" 4 (List.length tss) in

  let tc_object_counts =
    A.test_case "Object counts" `Quick @@ fun () ->
    let ls = Map.layers m in
    A.(check int) "equal" 4 (List.length ls) ;
    let l2 = List.find (fun l -> Layer.id l = 2) ls in
    A.(check int) "equal" 15 (List.length (Layer.objects l2)) ;
    let l4 = List.find (fun l -> Layer.id l = 4) ls in
    A.(check int) "equal" 13 (List.length (Layer.objects l4)) ;
    let ls' = match Layer.variant l4 with `Group g -> g | _ -> assert false in
    let l5 = List.find (fun l -> Layer.id l = 5) ls' in
    A.(check int) "equal" 6 (List.length (Layer.objects l5)) ;
    let l6 = List.find (fun l -> Layer.id l = 6) ls' in
    A.(check int) "equal" 2 (List.length (Layer.objects l6)) ;
    let l7 = List.find (fun l -> Layer.id l = 7) ls' in
    A.(check int) "equal" 5 (List.length (Layer.objects l7)) ;
    A.(check int) "equal" 28 (List.length objects) in

  let tc_object_gids =
    A.test_case "Object GID remapping" `Quick @@ fun () ->
    A.(check (module Tile)) "equal" (ts_tile tsx_coll1 0) (o_tile (o 3)) ;
    A.(check (module Tile)) "equal" (ts_tile tsx_coll1 1) (o_tile (o 7)) ;
    A.(check (module Tile)) "equal" (ts_tile tsx_coll1 2) (o_tile (o 18)) ;
    A.(check (module Tile)) "equal" (ts_tile tsx_alt_coll1 0) (o_tile (o 34)) ;
    A.(check (module Tile)) "equal" (ts_tile tsx_alt_coll1 1) (o_tile (o 35)) ;
    A.(check (module Tile)) "equal" (ts_tile tsx_alt_coll1 2) (o_tile (o 37))
  in

  let tc_tile_gids =
    A.test_case "Tile data GID remapping" `Quick @@ fun () ->
    let l = List.find (fun l -> Layer.id l = 1) (Map.layers m) in
    let tl =
      match Layer.variant l with `Tilelayer tl -> tl | _ -> assert false in
    let gid_at col row = Layer.Tilelayer.gid_at tl ~col ~row in
    let tile_at col row = get_tile_exn (gid_at col row) in
    A.(check (module Gid)) "equal" (gid_at 0 0) (Gid.make 0) ;
    A.(check (module Tile)) "equal" (ts_tile tsx_single1 0) (tile_at 1 0) ;
    A.(check (module Tile)) "equal" (ts_tile tsx_single1 1) (tile_at 2 0) ;
    A.(check (module Tile)) "equal" (ts_tile tsx_single1 2) (tile_at 3 0) ;
    A.(check (module Tile)) "equal" (ts_tile tsx_single1 3) (tile_at 4 0) ;
    A.(check (module Tile)) "equal" (ts_tile tsx_single1 4) (tile_at 5 0) ;
    A.(check (module Tile)) "equal" (ts_tile tsx_single1 5) (tile_at 6 0) ;
    A.(check (module Tile)) "equal" (ts_tile tsx_single1 6) (tile_at 7 0) ;
    A.(check (module Tile)) "equal" (ts_tile tsx_single1 7) (tile_at 8 0) ;
    A.(check (module Tile)) "equal" (ts_tile tsx_alt_single1 0) (tile_at 9 0) ;
    A.(check (module Tile)) "equal" (ts_tile tsx_alt_single1 1) (tile_at 10 0) ;
    A.(check (module Tile)) "equal" (ts_tile tsx_alt_single1 2) (tile_at 11 0) ;
    A.(check (module Tile)) "equal" (ts_tile tsx_alt_single1 3) (tile_at 12 0) ;
    A.(check (module Tile)) "equal" (ts_tile tsx_alt_single1 4) (tile_at 13 0) ;
    A.(check (module Tile)) "equal" (ts_tile tsx_alt_single1 5) (tile_at 14 0) ;
    A.(check (module Tile)) "equal" (ts_tile tsx_alt_single1 6) (tile_at 15 0) ;
    A.(check (module Tile)) "equal" (ts_tile tsx_alt_single1 7) (tile_at 16 0)
  in

  let tc_templates =
    A.test_case "Templates" `Quick @@ fun () ->
    A.(check (float 1e-3)) "equal" 45. (Object.rotation (o 6)) ;
    A.(check (float 1e-3)) "equal" 28. (Object.width (o 6)) ;
    A.(check (float 1e-3)) "equal" 16. (Object.height (o 6)) ;
    A.(check (module Tile)) "equal" (ts_tile tsx_coll1 0) (o_tile (o 6)) ;
    A.(check (float 1e-3)) "equal" 45. (Object.rotation (o 19)) ;
    A.(check (float 1e-3)) "equal" 56. (Object.width (o 19)) ;
    A.(check (float 1e-3)) "equal" 32. (Object.height (o 19)) ;
    A.(check (module Tile)) "equal" (ts_tile tsx_coll1 2) (o_tile (o 19)) in

  ( "All",
    [tc_tilesets; tc_object_counts; tc_object_gids; tc_tile_gids; tc_templates]
  )

let () = A.run "Map" [test_all]
