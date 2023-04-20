open Tmx__

let loader = Loader.make ~root:(Sys.getcwd ())
open (val loader)

let tc_basic =
  Alcotest.test_case "fixed1.tmx" `Quick @@ fun () ->
  let m = load_map_xml_exn "data/fixed1.tmx" in
  (* Tilesets *)
  let tss = Map.tilesets m in
  Alcotest.(check int) "equal" 4 (List.length tss) ;
  (* Layers & object counts *)
  let ls = Map.layers m in
  Alcotest.(check int) "equal" 4 (List.length ls) ;
  let l2 = List.find (fun l -> Layer.id l = 2) ls in
  Alcotest.(check int) "equal" 6 (List.length (Layer.objects l2)) ;
  let l4 = List.find (fun l -> Layer.id l = 4) ls in
  Alcotest.(check int) "equal" 6 (List.length (Layer.objects l4)) ;
  let ls' = match Layer.variant l4 with `Group ls -> ls | _ -> assert false in
  let l5 = List.find (fun l -> Layer.id l = 5) ls' in
  Alcotest.(check int) "equal" 3 (List.length (Layer.objects l5)) ;
  let l6 = List.find (fun l -> Layer.id l = 6) ls' in
  Alcotest.(check int) "equal" 2 (List.length (Layer.objects l6)) ;
  let l7 = List.find (fun l -> Layer.id l = 7) ls' in
  Alcotest.(check int) "equal" 1 (List.length (Layer.objects l7)) ;
  Alcotest.(check int) "equal" 12 (List.length (Map.objects m)) ;
  (* Template objects *)
  let o6 = Option.get (Map.get_object m 6) in
  Alcotest.(check (float 1e-3)) "equal" 45. (Object.rotation o6) ;
  Alcotest.(check (float 1e-3)) "equal" 28. (Object.width o6) ;
  Alcotest.(check (float 1e-3)) "equal" 16. (Object.height o6) ;
  let o19 = Option.get (Map.get_object m 19) in
  Alcotest.(check (float 1e-3)) "equal" 45. (Object.rotation o19) ;
  Alcotest.(check (float 1e-3)) "equal" 56. (Object.width o19) ;
  Alcotest.(check (float 1e-3)) "equal" 32. (Object.height o19)

let test_all = ("All", [tc_basic])

let () = Alcotest.run "Map" [test_all]
