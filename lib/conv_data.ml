open Ezxmlm
open Util.Option_infix

let member_opt k nodes =
  try Some (member k nodes) with Tag_not_found _ -> None
let member_with_attr_opt k nodes =
  try Some (member_with_attr k nodes) with Tag_not_found _ -> None
let get_attr_opt k attrs = try Some (get_attr k attrs) with Not_found -> None

let get_encoding attrs =
  get_attr_opt "encoding" attrs >|= function
  | "base64" -> `Base64
  | "csv" -> `Csv
  | _ -> failwith "TODO"

let get_compression attrs =
  get_attr_opt "compression" attrs >|= function
  | "gzip" -> `Gzip
  | "zlib" -> `Zlib
  | "zstd" -> `Zstd
  | _ -> failwith "TODO"

let decode_b64_plain (_, nodes) : bytes =
  data_to_string nodes |> Base64.decode_exn |> Bytes.unsafe_of_string

let decode_b64_gzip (_, nodes) : bytes =
  data_to_string nodes |> Base64.decode_exn |> Ezgzip.decompress |> function
  | Ok s -> Bytes.unsafe_of_string s
  | Error e ->
      Format.kasprintf failwith "TODO: Gzip decompression failed: %a"
        Ezgzip.pp_gzip_error e

let decode_b64_zlib (_, nodes) : bytes =
  data_to_string nodes |> Base64.decode_exn |> Ezgzip.Z.decompress ~header:true
  |> function
  | Ok s -> Bytes.unsafe_of_string s
  | Error e ->
      Format.kasprintf invalid_arg "TODO: Zlib decompression failed: %a"
        Ezgzip.Z.pp_zlib_error e

let decode_b64_zstd _ = failwith "TODO: Zstd compression not supported"

let decode_csv (_, nodes) : bytes =
  let frags : string list = data_to_string nodes |> String.split_on_char ',' in
  let bytes = Bytes.create @@ (List.length frags * 4) in
  List.iteri
    (fun i s ->
      let gid = Int32.of_string @@ String.trim s in
      Bytes.set_int32_le bytes (i * 4) gid )
    frags ;
  bytes

let decode_plain (_, nodes) : bytes =
  let nodes = members_with_attr "tile" nodes in
  let bytes = Bytes.create @@ (List.length nodes * 4) in
  List.iteri
    (fun i (attrs, _) ->
      let gid = get_attr_opt "gid" attrs >|= Int32.of_string |? 0l in
      Bytes.set_int32_le bytes (i * 4) gid )
    nodes ;
  bytes

let decode ~encoding ~compression =
  match (encoding, compression) with
  | Some `Base64, None -> decode_b64_plain
  | Some `Base64, Some `Gzip -> decode_b64_gzip
  | Some `Base64, Some `Zlib -> decode_b64_zlib
  | Some `Base64, Some `Zstd -> decode_b64_zstd
  | Some `Csv, None -> decode_csv
  | None, None -> decode_plain
  | _ -> failwith "TODO: Compression not supported with this encoding"

let conv_int32s_to_native bs : unit =
  if Sys.big_endian then
    for i = 0 to (Bigstringaf.length bs / 4) - 1 do
      Bigstringaf.(set_int32_be bs i @@ get_int32_le bs i)
    done

let tile_data_of_xml ~dims:(w, h) ~chunked ((attrs, nodes) as xml) =
  let encoding = get_encoding attrs in
  let compression = get_compression attrs in
  let decode = decode ~encoding ~compression in
  let bs = Bigstringaf.create (w * h * 4) in
  let data = Data.make ?encoding ?compression bs in
  let () =
    if chunked then
      let xmls = members_with_attr "chunk" nodes in
      Fun.flip List.iter xmls @@ fun ((attrs, _) as xml) ->
      let src = decode xml in
      let src_w = get_attr "width" attrs |> int_of_string in
      let src_h = get_attr "height" attrs |> int_of_string in
      let x = get_attr "x" attrs |> int_of_string in
      let y = get_attr "y" attrs |> int_of_string in
      let len = min src_w (w - x) * 4 in
      for row = 0 to min src_h (h - y) - 1 do
        let src_off = row * src_w * 4 in
        let dst_off = (x + ((y + row) * w)) * 4 in
        Bigstringaf.blit_from_bytes src ~src_off bs ~dst_off ~len
      done
    else
      let src = decode xml in
      let (src_off as dst_off) = 0 in
      let len = w * h * 4 in
      Bigstringaf.blit_from_bytes src ~src_off bs ~dst_off ~len in
  conv_int32s_to_native bs ; data

let image_data_of_xml ((attrs, _) as xml) =
  let encoding = get_encoding attrs in
  let compression = get_compression attrs in
  let decode = decode ~encoding ~compression in
  let src = Bytes.unsafe_to_string @@ decode xml in
  let off = 0 in
  let len = String.length src in
  let bs = Bigstringaf.of_string ~off ~len src in
  Data.make ?encoding ?compression bs
