open Tmx__
open Nonbasic.Make ()

module P = Property

let prop name value = P.make ~name ~value ()
let prop' name pt value = P.make ~name ~propertytype:pt ~value ()

let class_ name members : Customtype.t =
  let variant = `Class (Class.make ~useas:[`Layer; `Property] ~members) in
  Customtype.make ~id:1 ~name ~variant

let customtypes =
  [ class_ "c1"
      [ prop "c1p1" (`Int 777); prop "c1p2" (`Float 1.23);
        prop "c1p3" (`Int 456) ];
    class_ "c2"
      [ prop "c2p1" (`String "klm"); prop "c2p2" (`Object 5);
        prop' "c2p3" "c1"
          (`Class [prop "c1p2" (`Float 7.89); prop "c1p3" (`Float 654.)]) ];
    class_ "c3"
      [ prop' "c3p1" "c2"
          (`Class
            [ prop "c2p2" (`Float 6.);
              prop "c2p3" (`Class [prop "c1p3" (`Float 543.)]) ] ) ] ]

let () =
  let load ct = State.update (Context.add_customtype_exn ct) in
  run_context (State.iter_list load customtypes)

let check_subprop t k v =
  let t' = P.get_property k t in
  Alcotest.(check (option (module P.Value))) "equal" v (Option.map P.value t')

let tc_simple =
  Alcotest.test_case "Simple" `Quick @@ fun () ->
  let t =
    prop' "x" "c1"
      (`Class [prop "c1p2" (`Float 2.34); prop "c1p3" (`Float 567.)]) in
  check_subprop t "c1p1" (Some (`Int 777)) ;
  check_subprop t "c1p2" (Some (`Float 2.34)) ;
  check_subprop t "c1p3" (Some (`Int 567)) ;
  check_subprop t "extra" None

let tc_nested =
  Alcotest.test_case "Nested" `Quick @@ fun () ->
  let t =
    prop' "x" "c2"
      (`Class
        [ prop "c2p2" (`Float 6.);
          prop "c2p3"
            (`Class
              [ prop "c1p2" (`Float 8.76); prop "c1p3" (`Float 9.);
                prop "extra" (`String "xyz") ] ) ] ) in
  check_subprop t "c2p1" (Some (`String "klm")) ;
  check_subprop t "c2p2" (Some (`Object 6)) ;
  let t' = P.get_property_exn "c2p3" t in
  Alcotest.(check (option string)) "equal" (Some "c1") (P.propertytype t') ;
  check_subprop t' "c1p1" (Some (`Int 777)) ;
  check_subprop t' "c1p2" (Some (`Float 8.76)) ;
  check_subprop t' "c1p3" (Some (`Int 9)) ;
  check_subprop t' "extra" None

let tc_deeply_nested =
  Alcotest.test_case "Deeply nested" `Quick @@ fun () ->
  let t =
    prop' "x" "c3"
      (`Class
        [ prop "c3p1"
            (`Class
              [ prop "c2p1" (`String "ghi");
                prop "c2p3"
                  (`Class
                    [prop "c1p1" (`Float 888.); prop "extra" (`Bool true)] ) ]
              ) ] ) in
  let t' = P.get_property_exn "c3p1" t in
  Alcotest.(check (option string)) "equal" (Some "c2") (P.propertytype t') ;
  check_subprop t' "c2p1" (Some (`String "ghi")) ;
  check_subprop t' "c2p2" (Some (`Object 6)) ;
  let t'' = P.get_property_exn "c2p3" t' in
  Alcotest.(check (option string)) "equal" (Some "c1") (P.propertytype t'') ;
  check_subprop t'' "c1p1" (Some (`Int 888)) ;
  check_subprop t'' "c1p2" (Some (`Float 7.89)) ;
  check_subprop t'' "c1p3" (Some (`Int 543)) ;
  check_subprop t'' "extra" None

let test_inherit = ("Inheritance", [tc_simple; tc_nested; tc_deeply_nested])

let () = Alcotest.run "Property" [test_inherit]
