(* TODO: find a better way to expose internal modules to tests *)
open Tmx__
open Basic

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

  ("Duplicate customtypes", [tc_dup_ct_allow; tc_dup_ct_disallow])

let test_tile =
  let ts1 =
    Tileset.make ~name:"ts1" ~columns:1 ~variant:`Collection
      Tile.[make ~id:0 (); make ~id:1 (); make ~id:3 ()] in
  let ts2 =
    Tileset.make ~name:"ts2" ~columns:1 ~variant:`Collection
      Tile.[make ~id:0 ~class_:"x" (); make ~id:1 ~class_:"y" ()] in
  let objects =
    List.init 8 @@ fun id -> Object.make ~id ~shape:(`Tile (Gid.make id)) ()
  in
  let m =
    let layers =
      let og = Layer.Objectgroup.make ~objects () in
      let l = Layer.make ~variant:(`Objectgroup og) () in
      [l] in
    Map.make ~version:"0.0" ~width:1 ~height:1 ~tilewidth:16 ~tileheight:16
      ~tilesets:[(1, "ts1"); (5, "ts2")]
      ~layers ~variant:`Orthogonal () in
  let t =
    C.default
    |> C.add_tileset_exn "ts1" ts1
    |> C.add_tileset_exn "ts2" ts2
    |> C.add_map_exn "m" m in

  let tile ts id = Tileset.get_tile ts id in
  let object_ id = List.find (fun o -> Object.id o = id) objects in

  let check o tile_exp =
    let gid =
      match Object.shape o with `Tile gid -> gid | _ -> assert false in
    let tile = C.get_object_tile o gid t in
    Alcotest.(check (option (module Tile))) "equal" tile_exp tile in

  let tc_basic =
    Alcotest.test_case "Find object tile" `Quick @@ fun () ->
    check (object_ 1) (tile ts1 0) ;
    check (object_ 2) (tile ts1 1) ;
    check (object_ 4) (tile ts1 3) ;
    check (object_ 5) (tile ts2 0) ;
    check (object_ 6) (tile ts2 1) in

  let tc_invalid =
    Alcotest.test_case "Don't find tile of invalid objects" `Quick @@ fun () ->
    check (object_ 0) None ;
    check (object_ 3) None ;
    check (object_ 7) None ;
    let o_extra = Object.make ~id:99 ~shape:(`Tile (Gid.make 99)) () in
    check o_extra None in

  ("get_object_tile", [tc_basic; tc_invalid])

let () = Alcotest.run "State" [test_dup_ct; test_tile]
