(* Equivalent to Bigstringaf.t *)

type t =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
[@@deriving eq, ord, show]
