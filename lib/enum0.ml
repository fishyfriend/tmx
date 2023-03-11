module Storagetype = struct
  type t = [`Int | `String] [@@deriving eq, ord, show]
end

type storagetype = Storagetype.t

type t =
  {storagetype : Storagetype.t; valuesasflags : bool; values : string array}
[@@deriving eq, ord, show]

let make ~storagetype ~valuesasflags values =
  let values = Array.of_list values in
  {storagetype; valuesasflags; values}

let storagetype t = t.storagetype
let valuesasflags t = t.valuesasflags
let values t = Array.to_list t.values
