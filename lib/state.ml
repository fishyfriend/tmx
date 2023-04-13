type ('a, 'b) t = 'b -> 'a * 'b

let return x s = (x, s)
let bind t f s = t s |> fun (x, s') -> f x s'
let map t f = bind t @@ fun x -> return (f x)
let run t s = t s
let get () s = (s, s)
let set s _ = ((), s)
let read f = map (get ()) f
let update f = bind (get ()) @@ fun s -> set (f s)

let rec map_list f xs =
  match xs with
  | [] -> return []
  | x :: xs ->
      bind (f x) @@ fun y ->
      map (map_list f xs) @@ fun ys -> y :: ys

let rec iter_list f xs =
  match xs with
  | [] -> return ()
  | x :: xs -> bind (f x) @@ fun () -> iter_list f xs

let map_option f o =
  match o with Some x -> map (f x) Option.some | None -> return None

let iter_option f o = map (map_option f o) ignore

module Infix = struct let ( >>= ) t f = bind t f let ( >|= ) t f = map t f end
module Syntax = struct include Infix let ( let* ) = bind let ( let+ ) = map end
