type t [@@deriving eq, ord, show]

val make : ?tileset:int * string -> Object0.t -> t
val tileset : t -> (int * string) option
val object_ : t -> Object0.t
