module A = Alcotest

open Tmx__
open Core_generic

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

let m_fixed1 = with_xml_from_file "data/fixed1.tmx" Conv_xml.map_of_xml

let tc_map_tilelayer =
  A.test_case "Sanity check tilelayer" `Quick @@ fun () ->
  let l = Map.get_layer_exn m_fixed1 1 in
  let tl = match Layer.variant l with `Tilelayer x -> x | _ -> assert false in
  A.(check int) "equal" 24 (Layer.Tilelayer.width tl) ;
  A.(check int) "equal" 16 (Layer.Tilelayer.height tl) ;
  for col = 0 to 15 do
    A.(check int)
      "equal" (col mod 8)
      (Gid.id (Layer.Tilelayer.gid_at ~col ~row:8 tl))
  done

let tc_map_objectgroup =
  A.test_case "Sanity check objectgroup" `Quick @@ fun () ->
  check_layer_object_count ~m:m_fixed1 ~l:2 15 ;
  check_layer_object_count ~m:m_fixed1 ~l:4 13

let tc_map_group =
  A.test_case "Sanity check group" `Quick @@ fun () ->
  check_layer_object_count ~m:m_fixed1 ~l:5 6 ;
  check_layer_object_count ~m:m_fixed1 ~l:6 2 ;
  check_layer_object_count ~m:m_fixed1 ~l:7 5

let tc_map_general =
  A.test_case "General sanity check" `Quick @@ fun () ->
  A.(check int) "equal" 4 (List.length (Map.layers m_fixed1)) ;
  A.(check int) "equal" 28 (List.length (Map.objects m_fixed1))

let test_map =
  ("Map", [tc_map_tilelayer; tc_map_objectgroup; tc_map_group; tc_map_general])

let ts_single1 = with_xml_from_file "data/single1.tsx" Conv_xml.tileset_of_xml

let tc_ts_missing =
  A.test_case "Add missing tiles" `Quick @@ fun () ->
  let tiles = Tileset.tiles ts_single1 in
  A.(check int) "equal" 8 (List.length tiles) ;
  A.(check int) "equal" 8 (Tileset.tilecount ts_single1) ;
  ignore @@ Tileset.get_tile_exn ts_single1 2 ;
  ignore @@ Tileset.get_tile_exn ts_single1 7

let tc_ts_animation =
  A.test_case "Read tile animations" `Quick @@ fun () ->
  let tile = Tileset.get_tile_exn ts_single1 1 in
  A.(check int) "equal" 4 (List.length (Tile.animation tile))

let tc_ts_subimage =
  A.test_case "Calculate tile subimages" `Quick @@ fun () ->
  let img = "tileset.png" in
  check_tile_image ~ts:ts_single1 ~tile:0 ~pos:(2, 2) ~dims:(16, 12) img ;
  check_tile_image ~ts:ts_single1 ~tile:3 ~pos:(53, 2) ~dims:(16, 12) img ;
  check_tile_image ~ts:ts_single1 ~tile:4 ~pos:(2, 15) ~dims:(16, 12) img ;
  check_tile_image ~ts:ts_single1 ~tile:7 ~pos:(53, 15) ~dims:(16, 12) img

let tc_ts_reorder =
  A.test_case "Allow reordered tiles" `Quick @@ fun () ->
  let tile4 = Tileset.get_tile_exn ts_single1 4 in
  let tile5 = Tileset.get_tile_exn ts_single1 5 in
  A.(check (option string)) "equal" None (Tile.class_ tile4) ;
  A.(check (option string)) "equal" (Some "abc") (Tile.class_ tile5)

let test_tileset =
  ("Tileset", [tc_ts_missing; tc_ts_animation; tc_ts_subimage; tc_ts_reorder])

let () = A.run "Conversions" [test_map; test_tileset]
