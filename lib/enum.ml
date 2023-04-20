module Storagetype = struct
  type t = [`Int | `String] [@@deriving eq, ord, show {with_path = false}]
end

type storagetype = Storagetype.t

type t =
  {storagetype : Storagetype.t; valuesasflags : bool; values : string array}
[@@deriving eq, ord, show {with_path = false}]

let make ~storagetype ~valuesasflags values =
  let values = Array.of_list values in
  {storagetype; valuesasflags; values}

let storagetype t = t.storagetype
let valuesasflags t = t.valuesasflags
let values t = Array.to_list t.values

let read_as_int t v =
  let n = Array.length t.values in
  match (storagetype t, valuesasflags t, v) with
  | `Int, true, `Int x ->
      Some Int.(lognot 0 |> Fun.flip shift_left n |> lognot |> logand x)
  | `Int, false, `Int x when x < n -> Some x
  | `String, true, `String s ->
      let vs = String.split_on_char ',' s in
      let x, _ =
        Array.fold_right
          (fun v (x, m) ->
            let x' = if List.mem v vs then x lor m else x in
            (x', m lsr 1) )
          t.values
          (0, 1 lsl (n - 1)) in
      Some x
  | `String, false, `String s ->
      let rec aux i =
        if i < 0 then None else if s = t.values.(i) then Some i else aux (i - 1)
      in
      aux (n - 1)
  | _ -> None

let read_as_string t v =
  match (storagetype t, valuesasflags t, v) with
  | `Int, true, `Int x ->
      let vs, _ =
        Array.fold_right
          (fun v (vs, n) ->
            let vs' = if (1 lsl n) land x = 0 then vs else v :: vs in
            (vs', n - 1) )
          t.values
          ([], Array.length t.values - 1) in
      Some (String.concat "," vs)
  | `Int, false, `Int x when x < Array.length t.values -> Some t.values.(x)
  | `String, true, `String s ->
      let vs = String.split_on_char ',' s in
      values t
      |> List.filter (fun v -> List.mem v vs)
      |> String.concat "," |> Option.some
  | `String, false, `String s when Array.mem s t.values -> Some s
  | _ -> None

let read_as_alist t v =
  match (storagetype t, valuesasflags t, v) with
  | `Int, true, `Int x ->
      let alist =
        List.mapi (fun i v -> (v, (1 lsl i) land x <> 0)) (values t) in
      Some alist
  | `Int, false, `Int x when x < Array.length t.values ->
      let alist = List.mapi (fun i v -> (v, i = x)) (values t) in
      Some alist
  | `String, true, `String s ->
      let vs = String.split_on_char ',' s in
      let alist = List.map (fun v -> (v, List.mem v vs)) (values t) in
      Some alist
  | `String, false, `String s ->
      let alist, found =
        List.fold_right
          (fun v (alist, found) ->
            let found' = v = s in
            ((v, found') :: alist, found || found') )
          (values t) ([], false) in
      if found then Some alist else None
  | _ -> None
