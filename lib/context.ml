open Basic
open Util.Option.Infix

module String_map = Stdlib.Map.Make (String)

type t =
  { tilesets : (int * string * Tileset.t) list;
    templates : Template.t String_map.t;
    files : string String_map.t;
    customtypes : Customtype.t list String_map.t;
    maps : Map.t String_map.t }

let empty =
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

let get_class k t ~useas =
  String_map.find_opt k t.customtypes >>= fun cts ->
  List.find_map
    (fun ct ->
      match Customtype.variant ct with
      | `Class c when List.mem useas (Class.useas c) -> Some c
      | _ -> None )
    cts

let get_object_tile o gid t =
  (* TODO: this is quite inefficient *)
  let id = Gid.id gid in
  let maps = String_map.to_seq t.maps in
  Seq.find_map
    (fun (_, m) ->
      Object.id o |> Map.get_object m
      >>= (function o' when o' == o -> Some m | _ -> None)
      >|= Map.tilesets
      >>= List.find_map (fun (firstgid, fname) ->
              if firstgid <= id then
                let ts =
                  List.find_map
                    (fun (_, fname', ts) ->
                      if fname' = fname then Some ts else None )
                    t.tilesets in
                ts >>= Fun.flip Tileset.get_tile (id - firstgid)
              else None ) )
    maps

let add_tileset_exn k ts t =
  let rec last xs = List.(if length xs = 1 then hd xs else last (tl xs)) in
  { t with
    tilesets =
      (let firstgid =
         match t.tilesets with
         | [] -> 1
         | xs ->
             let firstgid0, _, ts0 = last xs in
             firstgid0 + Tileset.max_id ts0 in
       t.tilesets @ [(firstgid, k, ts)] ) }

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
              if has_conflict then Util.duplicate "customtype" name
              else Some (ct :: sibs) )
        t.customtypes }

let add kind key value map =
  String_map.update key
    (function Some _ -> Util.duplicate kind key | None -> Some value)
    map

let add_template_exn k e t = {t with templates = add "template" k e t.templates}
let add_file_exn k data t = {t with files = add "file" k data t.files}
let add_map_exn k m t = {t with maps = add "map" k m t.maps}

let remove_template k t = {t with templates = String_map.remove k t.templates}
let remove_file k t = {t with files = String_map.remove k t.files}
let remove_map k t = {t with maps = String_map.remove k t.maps}

let remove_tileset k t =
  {t with tilesets = List.filter (fun (_, k', _) -> k' <> k) t.tilesets}

let remove_customtypes k t =
  {t with customtypes = String_map.remove k t.customtypes}

let remove_class k ~useas t =
  let filter cts =
    List.filter
      (fun ct ->
        match Customtype.variant ct with
        | `Class c when List.mem useas (Class.useas c) -> false
        | _ -> true )
      cts in
  {t with customtypes = String_map.update k (Option.map filter) t.customtypes}
