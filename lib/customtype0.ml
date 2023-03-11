module Variant = struct
  type t = [`Class of Class0.t | `Enum of Enum0.t] [@@deriving eq, ord, show]
end

type variant = Variant.t

type t = {id : int; name : string; variant : Variant.t}
[@@deriving eq, ord, show, make]

let id t = t.id
let name t = t.name
let variant t = t.variant
