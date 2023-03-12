type t = {tileset : (int * string) option; object_ : Object0.t}
[@@deriving eq, ord, show]

let make ?tileset object_ =
  match Object0.template object_ with
  | Some _ -> Util.nested_template ()
  | None -> {tileset; object_}

let tileset t = t.tileset
let object_ t = t.object_
