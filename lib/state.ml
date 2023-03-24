include State_intf

open Util.Option_infix

(* TODO: Make State immutable. Loader will store and update a mutable reference
   to a State value as the final step in any processing. This way if an
   exception is raised during processing, the previous, known-good loader state
   remains intact. The current implementation may lead to loader states that
   are inconsistent, e.g., if only part of the resources load. *)

module Make () : S = struct
  let tilesets_list : (int * string * Tileset0.t) list ref = ref []

  let templates_tbl = Hashtbl.create 8
  let files_tbl = Hashtbl.create 8
  let customtypes_tbl = Hashtbl.create 8
  let maps_tbl = Hashtbl.create 8

  let tilesets = List.to_seq !tilesets_list
  let templates = Hashtbl.to_seq templates_tbl
  let files = Hashtbl.to_seq files_tbl
  let customtypes = Hashtbl.to_seq_values customtypes_tbl
  let maps = Hashtbl.to_seq maps_tbl

  let add kind tbl k v =
    match Hashtbl.find_opt tbl k with
    | Some _ -> Util.duplicate kind k
    | None -> Hashtbl.replace tbl k v

  let add_template_exn k te = add "template" templates_tbl k te
  let add_file_exn k data = add "file" files_tbl k data
  let add_map_exn k m = add "map" maps_tbl k m

  let add_tileset_exn k ts =
    let firstgid =
      match !tilesets_list with
      | [] -> 1
      | xs ->
          let firstgid0, _, ts0 = Util.last xs in
          firstgid0 + Tileset0.max_id ts0 in
    tilesets_list := !tilesets_list @ [(firstgid, k, ts)]

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

  let get_template_exn k = get "template" templates_tbl k
  let get_file_exn k = get "files" files_tbl k
  let get_customtype_exn k = get "customtype" customtypes_tbl k

  let get_tileset_exn k =
    match List.find_opt (fun (_, k', _) -> k' = k) !tilesets_list with
    | Some (firstgid, _, ts) -> (firstgid, ts)
    | None -> Util.not_found "tileset" k

  let get_tileset k = Util.protect_opt get_tileset_exn k
  let get_template k = Util.protect_opt get_template_exn k
  let get_file k = Util.protect_opt get_file_exn k
  let get_customtype k = Util.protect_opt get_customtype_exn k

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

  let get_class name ~useas = Util.protect_opt (get_class_exn ~useas) name

  (* TODO: this is quite inefficient *)
  let get_object_tile o gid =
    let id = Gid.id gid in
    let maps = Hashtbl.to_seq_values maps_tbl in
    Seq.find_map
      (fun m ->
        Object0.id o |> Map0.get_object m
        >>= (function o' when o' == o -> Some m | _ -> None)
        >|= Map0.tilesets
        >>= List.find_map (fun (firstgid, ts) ->
                if firstgid <= id then
                  let* _, ts' = get_tileset ts in
                  Tileset0.get_tile ts' (id - firstgid)
                else None ) )
      maps
end
