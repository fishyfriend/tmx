open Tmx__

module Core = Core.Make (struct
  open Core.Simple
  open Helpers.Simple

  include Aux

  let template =
    Template.make
      (Object.make ~name:"tem1" ~class_:"c0" ~shape:`Ellipse ~width:5.
         ~properties:
           [ prop "te1p1" (`String "te1p1"); prop "te1p2" (`String "te1p2");
             prop "w" (`String "te1_w"); prop "x" (`String "te1_x") ]
         () )

  let tile =
    Tile.make ~id:1 ~class_:"c0"
      ~properties:
        [ prop "ti1p1" (`String "ti1p1"); prop "ti1p2" (`String "ti1p2");
          prop "w" (`String "ti1_w"); prop "x" (`String "ti1_x");
          prop "y" (`String "ti1_y") ]
      ()

  let customtype =
    class_ "c1" [`Object]
      [ prop "c1p1" (`String "c1p1"); prop "c1p2" (`String "c1p2");
        prop "w" (`String "c1_w"); prop "x" (`String "c1_x");
        prop "y" (`String "c1_y"); prop "z" (`String "c1_z") ]

  let get_template k = if k = "tem1" then Some template else None
  let get_tile gid = if Gid.id gid = 1 then Some tile else None
  let get_customtypes k = if k = "c1" then [customtype] else []
end)

open Core
open Helpers.Make (Core)

module A = Alcotest
module O = Object

let tc_tem_inherit =
  A.test_case "Inherited attrs" `Quick @@ fun () ->
  let o = O.make ~template:"tem1" () in
  A.(check (option string)) "equal" (Some "c0") (O.class_ o) ;
  A.(check (module O.Shape)) "equal" `Ellipse (O.shape o) ;
  A.(check (float 1e-5)) "equal" 5. (O.width o)

let tc_tem_override =
  A.test_case "Overridden attrs" `Quick @@ fun () ->
  let o = O.make ~template:"tem1" ~class_:"c1" ~shape:`Rectangle ~width:6. () in
  A.(check (option string)) "equal" (Some "c1") (O.class_ o) ;
  A.(check (module O.Shape)) "equal" `Rectangle (O.shape o) ;
  A.(check (float 1e-5)) "equal" 6. (O.width o)

let tc_tem_props =
  A.test_case "Properties" `Quick @@ fun () ->
  let properties = [prop "te1p2" (`String "o_te1p2")] in
  let o = O.make ~template:"tem1" ~properties () in
  check_prop (module O) o "te1p1" (`String "te1p1") ;
  check_prop (module O) o "te1p2" (`String "o_te1p2")

let test_tem =
  ("Inherit from template", [tc_tem_inherit; tc_tem_override; tc_tem_props])

let tc_tile_class =
  A.test_case "Class" `Quick @@ fun () ->
  let o = O.make ~shape:(`Tile (Gid.make 1)) () in
  A.(check (option string)) "equal" (Some "c0") (O.class_ o) ;
  let o' = O.make ~class_:"c1" ~shape:(`Tile (Gid.make 1)) () in
  A.(check (option string)) "equal" (Some "c1") (O.class_ o')

let tc_tile_props =
  A.test_case "Properties" `Quick @@ fun () ->
  let properties = [prop "ti1p2" (`String "o_ti1p2")] in
  let o = O.make ~shape:(`Tile (Gid.make 1)) ~properties () in
  check_prop (module O) o "ti1p1" (`String "ti1p1") ;
  check_prop (module O) o "ti1p2" (`String "o_ti1p2")

let test_tile = ("Inherit from tile", [tc_tile_class; tc_tile_props])

let tc_class_props =
  A.test_case "Properties" `Quick @@ fun () ->
  let properties = [prop "c1p2" (`String "o_c1p2")] in
  let o = O.make ~class_:"c1" ~properties () in
  check_prop (module O) o "c1p1" (`String "c1p1") ;
  check_prop (module O) o "c1p2" (`String "o_c1p2")

let test_class = ("Inherit from class", [tc_class_props])

let tc_multi_props =
  A.test_case "Properties" `Quick @@ fun () ->
  let o =
    O.make ~class_:"c1"
      ~shape:(`Tile (Gid.make 1))
      ~template:"tem1"
      ~properties:[prop "w" (`String "o_w")]
      () in
  check_prop (module O) o "w" (`String "o_w") ;
  check_prop (module O) o "x" (`String "te1_x") ;
  check_prop (module O) o "y" (`String "ti1_y") ;
  check_prop (module O) o "z" (`String "c1_z")

let test_multi = ("Inherit from multiple", [tc_multi_props])

let () = A.run "Objects" [test_tem; test_tile; test_class; test_multi]
