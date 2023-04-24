(* TODO: find a better way to expose internal modules to tests *)
open Tmx__
open Core_generic

module C = Context

let test_dup_ct =
  let add_class id useas t =
    let name = string_of_int id in
    let variant = `Class (Class.make ~useas ~members:[]) in
    let ct = Customtype.make ~id ~name ~variant in
    C.add_customtype_exn ct t in

  let add_enum id t =
    let name = string_of_int id in
    let variant = `Enum (Enum.make ~storagetype:`Int ~valuesasflags:false []) in
    let ct = Customtype.make ~id ~name ~variant in
    C.add_customtype_exn ct t in

  let dup_ct name = Error.Error (`Duplicate ("customtype", name)) in

  let tc_dup_ct_allow =
    Alcotest.test_case "Allow non-conflicting dups" `Quick @@ fun () ->
    let t =
      C.default |> add_class 1 [`Object; `Tile] |> add_class 2 [`Property]
    in
    ignore @@ add_class 1 [`Property] t ;
    ignore @@ add_enum 1 t ;
    ignore @@ add_class 2 [`Object; `Tile] t in

  let tc_dup_ct_disallow =
    Alcotest.test_case "Disallow conflicting dups" `Quick @@ fun () ->
    let t =
      C.default |> add_class 1 [`Object; `Tile] |> add_class 2 [`Property]
    in
    Alcotest.check_raises "raises" (dup_ct "1") (fun () ->
        ignore @@ add_class 1 [`Object] t ) ;
    Alcotest.check_raises "raises" (dup_ct "2") (fun () ->
        ignore @@ add_class 2 [`Property] t ) ;
    Alcotest.check_raises "raises" (dup_ct "2") (fun () ->
        ignore @@ add_enum 2 t ) in

  ("Dup customtypes", [tc_dup_ct_allow; tc_dup_ct_disallow])

let test_tile =
  let ts1 =
    Tileset.make ~name:"ts1" ~columns:1 ~variant:`Collection
      Tile.[make ~id:0 (); make ~id:1 (); make ~id:3 ()] in
  let ts2 =
    Tileset.make ~name:"ts2" ~columns:1 ~variant:`Collection
      Tile.[make ~id:0 ~class_:"x" (); make ~id:1 ~class_:"y" ()] in
  let objects =
    Array.init 8 @@ fun id -> Object.make ~id ~shape:(`Tile (Gid.make id)) ()
  in
  let m =
    let og = Layer.Objectgroup.make ~objects:(Array.to_list objects) () in
    let l = Layer.make ~variant:(`Objectgroup og) () in
    Map.make ~version:"0.0" ~width:1 ~height:1 ~tilewidth:16 ~tileheight:16
      ~tilesets:[(1, "ts1"); (20, "ts2")]
      ~layers:[l] ~geometry:`Orthogonal () in
  let t =
    C.default
    |> C.add_tileset_exn "ts1" ts1
    |> C.add_tileset_exn "ts2" ts2
    |> C.add_map_exn "m" m in

  let tc_invariant =
    Alcotest.test_case "Tilesets sort by GID desc." `Quick @@ fun () ->
    let tss_exp = [(5, "ts2", ts2); (1, "ts1", ts1)] in
    Alcotest.(check (list (triple int string (module Tileset))))
      "equal" tss_exp (C.tilesets t) in

  let check_get_tile id tile_exp =
    let gid = Gid.make id in
    let tile = C.get_tile gid t in
    Alcotest.(check (option (module Tile))) "equal" tile_exp tile in

  let tc_valid =
    Alcotest.test_case "Find valid tiles" `Quick @@ fun () ->
    check_get_tile 1 (Tileset.get_tile ts1 0) ;
    check_get_tile 2 (Tileset.get_tile ts1 1) ;
    check_get_tile 4 (Tileset.get_tile ts1 3) ;
    check_get_tile 5 (Tileset.get_tile ts2 0) ;
    check_get_tile 6 (Tileset.get_tile ts2 1) in

  let tc_invalid =
    Alcotest.test_case "Don't find invalid tiles" `Quick @@ fun () ->
    check_get_tile 0 None ;
    check_get_tile 3 None ;
    check_get_tile 7 None ;
    check_get_tile 99 None in

  ("Tile lookup", [tc_invariant; tc_valid; tc_invalid])

let () = Alcotest.run "Context" [test_dup_ct; test_tile]
