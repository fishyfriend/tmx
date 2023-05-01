open Core.Simple
open Util.Option.Infix

module String_map = Stdlib.Map.Make (String)

(* Invariant : tilesets in decreasing order of firstgid *)
type t =
  { tilesets : (int * string * Tileset.t) list;
    templates : Template.t String_map.t;
    files : string String_map.t;
    customtypes : Customtype.t list String_map.t;
    maps : Map.t String_map.t }

let default =
  { tilesets = [];
    templates = String_map.empty;
    files = String_map.empty;
    customtypes = String_map.empty;
    maps = String_map.empty }

let tilesets t = t.tilesets
let templates t = String_map.bindings t.templates
let files t = String_map.bindings t.files
let customtypes t = String_map.bindings t.customtypes |> List.concat_map snd
let maps t = String_map.bindings t.maps

let get_template k t = String_map.find_opt k t.templates
let get_file k t = String_map.find_opt k t.files
let get_customtypes k t = String_map.find_opt k t.customtypes |? []
let get_map k t = String_map.find_opt k t.maps

let get_tileset k t =
  List.find_map
    (fun (firstgid, k', ts) -> if k' = k then Some (firstgid, ts) else None)
    t.tilesets

let get_tile gid t =
  let id = Gid.id gid in
  let pair =
    List.find_map
      (fun (firstgid, _, ts) ->
        if firstgid <= id then Some (id - firstgid, ts) else None )
      t.tilesets in
  pair >>= fun (id, ts) -> Tileset.get_tile ts id

let add_tileset_exn k ts t =
  let firstgid =
    match t.tilesets with
    | [] -> 1
    | (firstgid0, _, ts0) :: _ -> firstgid0 + Tileset.max_tile_id ts0 + 1 in
  let tilesets = (firstgid, k, ts) :: t.tilesets in
  {t with tilesets}

let add_customtype_exn ct t =
  let name = Customtype.name ct in
  { t with
    customtypes =
      String_map.update name
        (function
          | None -> Some [ct]
          | Some sibs ->
              let has_conflict =
                let useas ct : Class.useas list =
                  match Customtype.variant ct with
                  | `Class c -> Class.useas c
                  | `Enum _ -> [`Property] in
                let conjoint xs ys = List.exists (fun x -> List.mem x ys) xs in
                conjoint (List.concat_map useas sibs) (useas ct) in
              if has_conflict then Util.Error.duplicate "customtype" name
              else Some (ct :: sibs) )
        t.customtypes }

let add kind key value map =
  String_map.update key
    (function Some _ -> Util.Error.duplicate kind key | None -> Some value)
    map

let add_template_exn k e t = {t with templates = add "template" k e t.templates}
let add_file_exn k data t = {t with files = add "file" k data t.files}
let add_map_exn k m t = {t with maps = add "map" k m t.maps}

let remove_template k t = {t with templates = String_map.remove k t.templates}
let remove_file k t = {t with files = String_map.remove k t.files}
let remove_map k t = {t with maps = String_map.remove k t.maps}

let remove_tileset k t =
  {t with tilesets = List.filter (fun (_, k', _) -> k' <> k) t.tilesets}

let update_customtypes k f t =
  let update cts =
    match cts with
    | Some cts -> (match f cts with [] -> None | cts -> Some cts)
    | None -> None in
  {t with customtypes = String_map.update k update t.customtypes}

let remove_customtypes k t = update_customtypes k (fun _ -> []) t
