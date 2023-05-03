(* Property inheritance.

   This serves as a test of strict inheritance logic ([Props.make
   ~strict:true]). *)

open Tmx__

module Core = Core.Make (struct
  open Core.Simple
  open Helpers.Simple

  include Aux

  let customtypes =
    [ class_ "c1" [`Property]
        [ prop "c1p1" (`Int 777); prop "c1p2" (`Float 1.23);
          prop "c1p3" (`Int 456) ];
      class_ "c2" [`Property]
        [ prop "c2p1" (`String "klm"); prop "c2p2" (`Object 5);
          prop' "c2p3" "c1"
            (`Class [prop "c1p2" (`Float 7.89); prop "c1p3" (`Float 654.)]) ];
      class_ "c3" [`Property]
        [ prop' "c3p1" "c2"
            (`Class
              [ prop "c2p2" (`Float 6.);
                prop "c2p3" (`Class [prop "c1p3" (`Float 543.)]) ] ) ] ]

  let get_customtypes k =
    List.filter (fun ct -> Customtype.name ct = k) customtypes
end)

open Core
open Helpers.Make (Core)

module P = Property

let tc_property_simple =
  Alcotest.test_case "Simple" `Quick @@ fun () ->
  let p =
    prop' "p" "c1"
      (`Class [prop "c1p2" (`Float 2.34); prop "c1p3" (`Float 567.)]) in
  check_prop (module P) p "c1p1" (`Int 777) ;
  check_prop (module P) p "c1p2" (`Float 2.34) ;
  check_prop (module P) p "c1p3" (`Int 567) ;
  check_no_prop (module P) p "extra"

let tc_property_nested =
  Alcotest.test_case "Nested" `Quick @@ fun () ->
  let p =
    prop' "p" "c2"
      (`Class
        [ prop "c2p2" (`Float 6.);
          prop "c2p3"
            (`Class
              [ prop "c1p2" (`Float 8.76); prop "c1p3" (`Float 9.);
                prop "extra" (`String "xyz") ] ) ] ) in

  let c2p3 = P.get_property_exn "c2p3" p in
  Alcotest.(check (option string)) "equal" (Some "c1") (P.propertytype c2p3) ;
  check_prop (module P) c2p3 "c1p1" (`Int 777) ;
  check_prop (module P) c2p3 "c1p2" (`Float 8.76) ;
  check_prop (module P) c2p3 "c1p3" (`Int 9) ;
  check_no_prop (module P) c2p3 "extra"

let tc_property_deep =
  Alcotest.test_case "Deeply nested" `Quick @@ fun () ->
  let p =
    prop' "p" "c3"
      (`Class
        [ prop "c3p1"
            (`Class
              [ prop "c2p1" (`String "ghi");
                prop "c2p3"
                  (`Class
                    [prop "c1p1" (`Float 888.); prop "extra" (`Bool true)] ) ]
              ) ] ) in
  let c3p1 = P.get_property_exn "c3p1" p in
  Alcotest.(check (option string)) "equal" (Some "c2") (P.propertytype c3p1) ;
  check_prop (module P) c3p1 "c2p1" (`String "ghi") ;
  check_prop (module P) c3p1 "c2p2" (`Object 6) ;
  let c2p3 = P.get_property_exn "c2p3" c3p1 in
  Alcotest.(check (option string)) "equal" (Some "c1") (P.propertytype c2p3) ;
  check_prop (module P) c2p3 "c1p1" (`Int 888) ;
  check_prop (module P) c2p3 "c1p2" (`Float 7.89) ;
  check_prop (module P) c2p3 "c1p3" (`Int 543) ;
  check_no_prop (module P) c2p3 "extra"

let test_property =
  ( "Property inheritance",
    [tc_property_simple; tc_property_nested; tc_property_deep] )

let () = Alcotest.run "Property" [test_property]
