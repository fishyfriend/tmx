type t [@@deriving eq, ord, show]

module Useas : sig
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

val make : useas:Useas.t list -> members:Property0.t list -> t
val useas : t -> Useas.t list
val members : t -> Property0.t list
