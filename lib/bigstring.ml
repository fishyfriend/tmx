type t = (Bigstringaf.t[@opaque]) [@@deriving show]

let equal t t' = t = t'
let compare t t' = compare t t'
