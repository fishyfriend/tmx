module A = Alcotest

open Tmx__
open Core.Simple

let with_xml_from_file fname f =
  In_channel.with_open_text fname @@ fun ic ->
  Conv_xml.with_xml_from_channel ic f

let check_layer_object_count ~m ~l n_exp =
  let n = Map.get_layer_exn m l |> Layer.objects |> List.length in
  A.(check int) "equal" n_exp n

let get_tile_image_filename tile =
  let src = Tile.image tile |> Option.get |> Image.source in
  match src with `File x -> x | _ -> assert false

let check_tile_image ~ts ~tile ~pos:(x, y) ~dims:(w, h) fname =
  let tile = Tileset.get_tile_exn ts tile in
  A.(check int) "equal" x (Tile.x tile) ;
  A.(check int) "equal" y (Tile.y tile) ;
  A.(check (option int)) "equal" (Some w) (Tile.width tile) ;
  A.(check (option int)) "equal" (Some h) (Tile.height tile) ;
  A.(check string) "equal" fname (get_tile_image_filename tile)

let check_gid_at ~tl ~col ~row gid =
  A.check (module Gid) "equal" gid (Tilelayer.gid_at ~col ~row tl)

let m_fixed1 =
  with_xml_from_file "data/fixed1.tmx" Conv_xml.map_of_toplevel_xml

let tc_map_general =
  A.test_case "Load map" `Quick @@ fun () ->
  A.(check int) "equal" 24 (Map.width m_fixed1) ;
  A.(check int) "equal" 16 (Map.height m_fixed1) ;
  A.(check int) "equal" 4 (List.length (Map.layers m_fixed1)) ;
  A.(check int) "equal" 28 (List.length (Map.objects m_fixed1))

let tc_map_tilelayer =
  A.test_case "Load tilelayer" `Quick @@ fun () ->
  let l = Map.get_layer_exn m_fixed1 1 in
  let tl = match Layer.variant l with `Tilelayer x -> x | _ -> assert false in
  A.(check int) "equal" 24 (Tilelayer.width tl) ;
  A.(check int) "equal" 16 (Tilelayer.height tl) ;
  for col = 0 to 15 do
    check_gid_at ~tl ~col ~row:8 (Gid.make (col mod 8))
  done

let tc_map_objectgroup =
  A.test_case "Load objectgroup" `Quick @@ fun () ->
  check_layer_object_count ~m:m_fixed1 ~l:2 15 ;
  check_layer_object_count ~m:m_fixed1 ~l:4 13

let tc_map_group =
  A.test_case "Load group" `Quick @@ fun () ->
  check_layer_object_count ~m:m_fixed1 ~l:5 6 ;
  check_layer_object_count ~m:m_fixed1 ~l:6 2 ;
  check_layer_object_count ~m:m_fixed1 ~l:7 5

let tc_map_infinite =
  A.test_case "Load inf map" `Quick @@ fun () ->
  let m =
    with_xml_from_file "data/infinite1.tmx" Conv_xml.map_of_toplevel_xml in
  A.(check int) "equal" 48 (Map.width m) ;
  A.(check int) "equal" 64 (Map.height m) ;
  let tl =
    Map.get_layer_exn m 2 |> Layer.variant |> function
    | `Tilelayer tl -> tl
    | _ -> assert false in
  A.(check int) "equal" 48 (Tilelayer.width tl) ;
  A.(check int) "equal" 64 (Tilelayer.height tl) ;
  check_gid_at ~tl ~col:2 ~row:3 (Gid.make 5) ;
  check_gid_at ~tl ~col:17 ~row:20 (Gid.make 6) ;
  check_gid_at ~tl ~col:44 ~row:39 (Gid.make 7) ;
  check_gid_at ~tl ~col:21 ~row:60 (Gid.make 8) ;
  List.iter
    (fun (x, y) ->
      check_gid_at ~tl ~col:x ~row:y (Gid.make 1) ;
      check_gid_at ~tl ~col:(x + 16) ~row:y (Gid.make 2) ;
      check_gid_at ~tl ~col:x ~row:(y + 16) (Gid.make 3) ;
      check_gid_at ~tl ~col:(x + 16) ~row:(y + 16) (Gid.make 4) )
    [] ;
  let sum = ref 0 in
  for col = 0 to Tilelayer.width tl - 1 do
    for row = 0 to Tilelayer.height tl - 1 do
      if Tilelayer.gid_at tl ~col ~row <> Gid.make 0 then incr sum
    done
  done ;
  A.(check int) "equal" 20 !sum

let tc_map_err_path =
  A.test_case "Show error path" `Quick @@ fun () ->
  let err_exp =
    let path =
      [ "{root}"; "map"; "objectgroup"; "0"; "object"; "0"; "properties";
        "property"; "1"; "@type" ] in
    let msg = "invalid type: ITSABUG" in
    Error.Error (`Xml_parse (None, path, msg)) in
  A.check_raises "raises" err_exp @@ fun () ->
  ignore
    (with_xml_from_file "data/bad-obj-prop.tmx" Conv_xml.map_of_toplevel_xml)

let test_map =
  ( "Map",
    [ tc_map_tilelayer; tc_map_objectgroup; tc_map_group; tc_map_general;
      tc_map_infinite; tc_map_err_path ] )

let ts_single1 =
  with_xml_from_file "data/single1.tsx" Conv_xml.tileset_of_toplevel_xml

let tc_ts_missing =
  A.test_case "Add missing tiles" `Quick @@ fun () ->
  let tiles = Tileset.tiles ts_single1 in
  A.(check int) "equal" 8 (List.length tiles) ;
  A.(check int) "equal" 8 (Tileset.tilecount ts_single1) ;
  ignore @@ Tileset.get_tile_exn ts_single1 2 ;
  ignore @@ Tileset.get_tile_exn ts_single1 7

let tc_ts_animation =
  A.test_case "Load animations" `Quick @@ fun () ->
  let tile = Tileset.get_tile_exn ts_single1 1 in
  A.(check int) "equal" 4 (List.length (Tile.animation tile))

let tc_ts_subimage =
  A.test_case "Calc subimages" `Quick @@ fun () ->
  let img = "tileset.png" in
  check_tile_image ~ts:ts_single1 ~tile:0 ~pos:(2, 2) ~dims:(16, 12) img ;
  check_tile_image ~ts:ts_single1 ~tile:3 ~pos:(53, 2) ~dims:(16, 12) img ;
  check_tile_image ~ts:ts_single1 ~tile:4 ~pos:(2, 15) ~dims:(16, 12) img ;
  check_tile_image ~ts:ts_single1 ~tile:7 ~pos:(53, 15) ~dims:(16, 12) img

let tc_ts_reorder =
  A.test_case "Allow tile reorder" `Quick @@ fun () ->
  let tile4 = Tileset.get_tile_exn ts_single1 4 in
  let tile5 = Tileset.get_tile_exn ts_single1 5 in
  A.(check (option string)) "equal" None (Tile.class_ tile4) ;
  A.(check (option string)) "equal" (Some "abc") (Tile.class_ tile5)

let test_tileset =
  ("Tileset", [tc_ts_missing; tc_ts_animation; tc_ts_subimage; tc_ts_reorder])

let () = A.run "Conversions" [test_map; test_tileset]
