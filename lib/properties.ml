include Properties_intf

open Util.Option_infix

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
        (* Tiled hides extraneous subproperties; emulate this with [strict]. *)
        `Class (merge_property_lists ~strict:true ts0 ts)
    | _, v -> v in
  Property0.make ~name ?propertytype ~value ()

and merge_property_lists ~strict ts0 ts =
  match (ts0, ts) with
  | ts, [] | [], ts -> ts
  | t0 :: ts0', t :: ts' ->
    ( match String.compare (Property0.name t0) (Property0.name t) with
    | -1 -> t0 :: merge_property_lists ~strict ts0' ts
    | 1 ->
        if strict then merge_property_lists ~strict ts0 ts'
        else t :: merge_property_lists ~strict ts0 ts'
    | 0 -> merge_propertys t0 t :: merge_property_lists ~strict ts0' ts'
    | _ -> assert false )

module Make0 (T : PropsType) : S with type t := T.t = struct
  let find k ps = List.find_opt (fun p -> Property0.name p = k) ps

  let properties t =
    List.fold_left (merge_property_lists ~strict:false) [] (T.property_lists t)

  let get_property k t =
    List.fold_left
      (fun p plist ->
        match (p, find k plist) with
        | None, None -> p
        | p, None | None, p -> p
        | Some p, Some p' -> Some (merge_propertys p p') )
      None (T.property_lists t)

  let get_property_exn k t =
    match get_property k t with
    | Some p -> p
    | None -> Util.not_found "property" k
end

module Make (State : State_intf.S) (T : ClassType) : S with type t := T.t =
Make0 (struct
  include T

  let property_lists t =
    let class_props =
      class_ t
      >>= State.(read @@ Fun.flip get_class ~useas)
      >|= Class0.members |? [] in
    let own_props = properties t in
    [class_props; own_props]
end)
