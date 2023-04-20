(* Helpers for implementing property lookup *)

open Util.Option.Infix
open Types

module type S = Sigs.PropsT with type property := Types.property

type 'a t = (module S with type t = 'a)

let rec merge_propertys (p0 : property as 'a) (p : 'a) : 'a =
  let propertytype = p.propertytype >>? fun () -> p0.propertytype in
  let value =
    match (p0.value, p.value) with
    (* Promote raw JSON types as needed. *)
    | `Int _, `Float x -> `Int (int_of_float x)
    | `Color _, `String s -> `Color (Color.of_string s)
    | `File _, `String s -> `File s
    | `Object _, `Float x -> `Object (int_of_float x)
    | `Class ts0, `Class ts ->
        `Class (merge_property_lists ~strict:false ts0 ts)
    | _, v -> v in
  {p with propertytype; value}

and merge_property_lists ~strict (ps0 : property list as 'b) (ps : 'b) : 'b =
  match (ps0, ps) with
  | _, [] -> ps0
  | [], _ -> if strict then [] else ps
  | t0 :: ts0', t :: ts' ->
    ( match String.compare t0.name t.name with
    | -1 -> t0 :: merge_property_lists ~strict ts0' ps
    | 1 ->
        if strict then merge_property_lists ~strict ps0 ts'
        else t :: merge_property_lists ~strict ps0 ts'
    | 0 -> merge_propertys t0 t :: merge_property_lists ~strict ts0' ts'
    | _ -> assert false )

let make (type a) ~strict ~(property_lists : a -> property list list) : a t =
  ( module struct
    type t = a

    let properties t =
      List.fold_left (merge_property_lists ~strict) [] (property_lists t)

    (* TODO: It should be possible to rewrite [properties] and [get_property]
       so as to reuse the traversal/filtration logic *)
    let get_property (k : string) (t : t) : property option =
      let find k ps = List.find_opt (fun (p : property) -> p.name = k) ps in
      match property_lists t with
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
      | None -> Util.Error.not_found "property" k
  end )
