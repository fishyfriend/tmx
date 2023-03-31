open Util.Option.Infix

module String_map = Stdlib.Map.Make (String)

type t =
  { tilesets : (int * string * Tileset0.t) list;
    templates : Template0.t String_map.t;
    files : string String_map.t;
    customtypes : Customtype0.t list String_map.t;
    maps : Map0.t String_map.t }

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

let get kind key map =
  match String_map.find_opt key map with
  | Some v -> v
  | None -> Util.not_found kind key

let get_template_exn k t = get "template" k t.templates
let get_file_exn k t = get "files" k t.files
let get_customtype_exn k t = get "customtype" k t.customtypes |> List.hd

let get_tileset_exn k t =
  match List.find_opt (fun (_, k', _) -> k' = k) t.tilesets with
  | Some (firstgid, _, ts) -> (firstgid, ts)
  | None -> Util.not_found "tileset" k

let get_tileset k t = Util.protect_opt (get_tileset_exn k) t
let get_template k t = Util.protect_opt (get_template_exn k) t
let get_file k t = Util.protect_opt (get_file_exn k) t
let get_customtype k t = Util.protect_opt (get_customtype_exn k) t

let get_class k t ~useas =
  let cts = get "class" k t.customtypes in
  List.find_map
    (fun ct ->
      match Customtype0.variant ct with
      | `Class c when List.mem useas (Class0.useas c) -> Some c
      | _ -> None )
    cts

let get_class_exn k t ~useas =
  match get_class k t ~useas with
  | Some c -> c
  | None -> Util.not_found "class" k

(* TODO: this is quite inefficient *)
let get_object_tile o gid t =
  let id = Gid.id gid in
  let maps = String_map.to_seq t.maps in
  Seq.find_map
    (fun (_, m) ->
      Object0.id o |> Map0.get_object m
      >>= (function o' when o' == o -> Some m | _ -> None)
      >|= Map0.tilesets
      >>= List.find_map (fun (firstgid, fname) ->
              if firstgid <= id then
                let ts =
                  List.find_map
                    (fun (_, fname', ts) ->
                      if fname' = fname then Some ts else None )
                    t.tilesets in
                ts >>= Fun.flip Tileset0.get_tile (id - firstgid)
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
             firstgid0 + Tileset0.max_id ts0 in
       t.tilesets @ [(firstgid, k, ts)] ) }

let add_customtype_exn ct t =
  let name = Customtype0.name ct in
  { t with
    customtypes =
      String_map.update name
        (function
          | None -> Some [ct]
          | Some sibs ->
              let has_conflict =
                let useas ct : Class0.useas list =
                  match Customtype0.variant ct with
                  | `Class c -> Class0.useas c
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

let add_template_exn k te t =
  {t with templates = add "template" k te t.templates}

let add_file_exn k data t = {t with files = add "file" k data t.files}

let add_map_exn k m t = {t with maps = add "map" k m t.maps}
