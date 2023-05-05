include Props_intf

open Util.Option.Infix
open Types

let rec merge_propertys (p : property as 'a) (p0 : 'a) : 'a =
  let propertytype = p.propertytype >>? fun () -> p0.propertytype in
  let value =
    match (p.value, p0.value) with
    (* Promote raw JSON types as needed. *)
    | `Float x, `Int _ -> `Int (int_of_float x)
    | `String s, `Color _ -> `Color (Color.of_string s)
    | `String s, `File _ -> `File s
    | `Float x, `Object _ -> `Object (int_of_float x)
    | `Class ts, `Class ts0 ->
        `Class (merge_property_lists ~strict:false ts ts0)
    | v, _ -> v in
  {p with propertytype; value}

and merge_property_lists ~strict (ps : property list as 'b) (ps0 : 'b) : 'b =
  match (ps, ps0) with
  | [], ps0 -> ps0
  | ps, [] -> if strict then [] else ps
  | t :: ts', t0 :: ts0' ->
    ( match String.compare t.name t0.name with
    | 1 -> t0 :: merge_property_lists ~strict ps ts0'
    | -1 ->
        if strict then merge_property_lists ~strict ts' ps0
        else t :: merge_property_lists ~strict ts' ps0
    | 0 -> merge_propertys t t0 :: merge_property_lists ~strict ts' ts0'
    | _ -> assert false )

let make (type a) ~strict ~(property_lists : a -> property list list) : a t =
  ( module struct
    type t = a

    let find k ps = List.find_opt (fun (p : property) -> p.name = k) ps

    let own_properties t =
      match property_lists t with [] -> [] | ps :: _ -> ps

    let get_own_property k t = find k (own_properties t)

    let get_own_property_exn k t =
      match get_own_property k t with
      | Some p -> p
      | None -> Util.Error.not_found "property" k

    let properties t =
      List.fold_left (merge_property_lists ~strict) [] (property_lists t)

    let get_property (k : string) (t : t) : property option =
      let rec aux plists p =
        match plists with
        | [] -> None
        | plist :: plists ->
          ( match aux plists p with
          | None when strict && plists <> [] -> None
          | p ->
            ( match (find k plist, p) with
            | p', None | None, p' -> p'
            | Some p', Some p -> Some (merge_propertys p' p) ) ) in
      aux (property_lists t) None

    let get_property_exn k t =
      match get_property k t with
      | Some p -> p
      | None -> Util.Error.not_found "property" k
  end )
