open Tmx
open Data

let to_int32_list t =
  let bytes = bytes t in
  let n = Bytes.length bytes / 4 in
  List.init n @@ fun i -> Bytes.get_int32_ne bytes (i * 4)

let tc_list =
  Alcotest.test_case "From list of GIDs" `Quick @@ fun () ->
  let xs_exp = [1l; 0l; 2l; 0l; 0l; 3l; 0l; 0l; 0l; 4l] in
  let xs = of_int32_list xs_exp |> to_int32_list in
  Alcotest.(check (list int32)) "equal" xs_exp xs

let tc_csv =
  Alcotest.test_case "CSV encoding" `Quick @@ fun () ->
  let xs_exp = [1l; 0l; 2l; 0l; 0l; 3l; 0l; 0l; 0l; 4l] in
  let xs = of_string ~encoding:`Csv "1,0,2,0,0,\n3,0,0,0,4" |> to_int32_list in
  Alcotest.(check (list int32)) "equal" xs_exp xs

let tc_base64 =
  Alcotest.test_case "Base64 encoding, uncompressed" `Quick @@ fun () ->
  let xs_exp = [1l; 0l; 2l; 0l; 0l; 3l; 0l; 0l; 0l; 4l] in
  let xs =
    of_string ~encoding:`Base64
      "AQAAAAAAAAACAAAAAAAAAAAAAAADAAAAAAAAAAAAAAAAAAAABAAAAA=="
    |> to_int32_list in
  Alcotest.(check (list int32)) "equal" xs_exp xs

let tc_base64_gzip =
  Alcotest.test_case "Base64 encoding, gzip compression" `Quick @@ fun () ->
  let xs_exp = [1l; 0l; 2l; 0l; 0l; 3l; 0l; 0l; 0l; 4l] in
  let xs =
    of_string ~encoding:`Base64 ~compression:`Gzip
      "H4sIAAAAAAAA/2NkgAAmBgRgZkAFLEAMAH54pl4oAAAA"
    |> to_int32_list in
  Alcotest.(check (list int32)) "equal" xs_exp xs

let tc_base64_zlib =
  Alcotest.test_case "Base64 encoding, zlib compression" `Quick @@ fun () ->
  let xs_exp = [1l; 0l; 2l; 0l; 0l; 3l; 0l; 0l; 0l; 4l] in
  let xs =
    of_string ~encoding:`Base64 ~compression:`Zlib
      "eJxjZIAAJgYEYGZABSxADAAA3AAL"
    |> to_int32_list in
  Alcotest.(check (list int32)) "equal" xs_exp xs

let t_tile =
  ("Tile data", [tc_list; tc_csv; tc_base64; tc_base64_gzip; tc_base64_zlib])

let () = Alcotest.run "Data" [t_tile]
