module Flags = struct
  type t = int32 [@@deriving eq, ord, show]

  let ( + ) t t' = Int32.logor t t'
  let test t t' = Int32.logand t t' <> 0l
  let flip_horizontal = Int32.shift_left 1l 31
  let flip_vertical = Int32.shift_left 1l 30
  let (flip_diagonal as rotate_60) = Int32.shift_left 1l 29
  let rotate_120 = Int32.shift_left 1l 28
  let all = flip_horizontal + flip_vertical + flip_diagonal + rotate_120
  let empty = 0l
end

type t = int32 [@@deriving eq, show]

let max_id = Int32.(to_int @@ lognot Flags.all)

let make ?(flags = Flags.empty) id =
  if id < 0 || id > max_id then Util.invalid_arg "id" (string_of_int id) ;
  Int32.(logor flags @@ of_int id)

let id t = Int32.(to_int @@ logand t @@ lognot Flags.all)
let flags t = Int32.logand t Flags.all
let of_int32 x = x
let to_int32 t = t

let compare t t' =
  match Int.compare (id t) (id t') with
  | 0 -> Flags.compare (flags t) (flags t')
  | x -> x
