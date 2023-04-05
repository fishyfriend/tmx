module type S = Sigs.PropsT with type property := Property0.t

type 'a t = (module S with type t = 'a)

let rec merge_propertys t0 t =
  let name = Property0.name t in
  let propertytype =
    match Property0.propertytype t with
    | Some _ as pt -> pt
    | None -> Property0.propertytype t0 in
  let value =
    match Property0.(value t0, value t) with
    (* Promote raw JSON types as needed. *)
    | `Int _, `Float x -> `Int (int_of_float x)
    | `Color _, `String s -> `Color (Color.of_string s)
    | `File _, `String s -> `File s
    | `Object _, `Float x -> `Object (int_of_float x)
    | `Class ts0, `Class ts ->
        `Class (merge_property_lists ~strict:false ts0 ts)
    | _, v -> v in
  Property0.make ~name ?propertytype ~value ()

and merge_property_lists ~strict ts0 ts =
  match (ts0, ts) with
  | _, [] -> ts0
  | [], _ -> if strict then [] else ts
  | t0 :: ts0', t :: ts' ->
    ( match String.compare (Property0.name t0) (Property0.name t) with
    | -1 -> t0 :: merge_property_lists ~strict ts0' ts
    | 1 ->
        if strict then merge_property_lists ~strict ts0 ts'
        else t :: merge_property_lists ~strict ts0 ts'
    | 0 -> merge_propertys t0 t :: merge_property_lists ~strict ts0' ts'
    | _ -> assert false )

let make_deep ~strict (type a) (get_property_lists : a -> Property0.t list list)
    : a t =
  ( module struct
    type t = a

    let properties t =
      List.fold_left (merge_property_lists ~strict) [] (get_property_lists t)

    (* TODO: It should be possible to rewrite [properties] and [get_property]
       so as to reuse the traversal/filtration logic *)
    let get_property k t =
      let find k ps = List.find_opt (fun p -> Property0.name p = k) ps in
      match get_property_lists t with
      | [] -> None
      | plist :: plists ->
        ( match (strict, find k plist) with
        | true, None -> None
        | _, p ->
            List.fold_left
              (fun p plist ->
                match (p, find k plist) with
                | p', None | None, p' -> p'
                | Some p, Some p' -> Some (merge_propertys p p') )
              p plists )

    let get_property_exn k t =
      match get_property k t with
      | Some p -> p
      | None -> Util.not_found "property" k
  end )

let make_shallow (get_properties : 'a -> Property0.t list) : 'a t =
  make_deep ~strict:false @@ fun t -> [get_properties t]
