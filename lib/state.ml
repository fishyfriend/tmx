include State_intf

module Make () : S = struct
  let tilesets_tbl = Hashtbl.create 8
  let templates_tbl = Hashtbl.create 8
  let files_tbl = Hashtbl.create 8
  let customtypes_tbl = Hashtbl.create 8

  let tilesets = Hashtbl.to_seq tilesets_tbl
  let templates = Hashtbl.to_seq templates_tbl
  let files = Hashtbl.to_seq files_tbl
  let customtypes = Hashtbl.to_seq_values customtypes_tbl

  let add kind tbl k v =
    match Hashtbl.find_opt tbl k with
    | Some _ -> Util.duplicate kind k
    | None -> Hashtbl.replace tbl k v

  let add_tileset_exn k ts = add "tileset" tilesets_tbl k ts
  let add_template_exn k te = add "template" templates_tbl k te
  let add_file_exn k data = add "file" files_tbl k data

  let add_customtype_exn ct =
    let name = Customtype0.name ct in
    let has_conflict =
      let get_useas ct =
        match Customtype0.variant ct with
        | `Class c -> Class0.useas c
        | `Enum _ -> [`Property] in
      let useas = get_useas ct in
      let possible_conflicts = Hashtbl.find_all customtypes_tbl name in
      List.exists
        (fun ct' ->
          (not (Customtype0.equal ct' ct))
          && List.exists (fun u -> List.mem u (get_useas ct')) useas )
        possible_conflicts in
    if has_conflict then Util.duplicate "customtype" name
    else Hashtbl.replace customtypes_tbl name ct

  let get kind tbl k =
    match Hashtbl.find_opt tbl k with
    | Some v -> v
    | None -> Util.not_found kind k

  let get_tileset_exn k = get "tileset" tilesets_tbl k
  let get_template_exn k = get "template" templates_tbl k
  let get_file_exn k = get "files" files_tbl k
  let get_customtype_exn k = get "customtype" customtypes_tbl k

  let with_file fname f =
    if Sys.file_exists fname then In_channel.with_open_text fname f
    else Util.file_not_found fname

  let load_tileset_xml_exn fname =
    with_file fname @@ fun ic ->
    Conv_xml.(with_xml_from_channel ic tileset_of_xml)
    |> Util.tap (add_tileset_exn fname)

  let load_template_xml_exn fname =
    with_file fname @@ fun ic ->
    Conv_xml.(with_xml_from_channel ic template_of_xml)
    |> Util.tap (add_template_exn fname)

  let load_file_exn fname =
    with_file fname In_channel.input_all |> Util.tap (add_file_exn fname)

  let load_customtypes_json_exn fname =
    with_file fname @@ fun ic ->
    Conv_json.(with_json_from_channel ic customtypes_of_json)
    |> Util.tap (List.iter add_customtype_exn)

  let load_map_xml_exn fname =
    with_file fname @@ fun ic -> Conv_xml.(with_xml_from_channel ic map_of_xml)

  let get_class_exn name ~useas =
    let c =
      let cts = Hashtbl.find_all customtypes_tbl name in
      List.find_map
        (fun ct ->
          match Customtype0.variant ct with
          | `Class c when List.mem useas (Class0.useas c) -> Some c
          | _ -> None )
        cts in
    match c with Some c -> c | None -> Util.not_found "class" name
end
