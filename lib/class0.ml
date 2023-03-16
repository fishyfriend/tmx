module Useas = struct
  type t =
    [ `Property
    | `Map
    | `Layer
    | `Object
    | `Tile
    | `Tileset
    | `Wangcolor
    | `Wangset ]
  [@@deriving eq, ord, show]
end

type useas = Useas.t

type t = {useas : Useas.t list; members : Property0.t list}
[@@deriving eq, ord, show]

let make ~useas ~members =
  let members = List.sort_uniq Property0.compare members in
  {useas; members}

let useas t = t.useas
let members t = t.members
