type t [@@deriving eq, ord, show]

val make : ?tileset:int * string -> object_:Object0.t -> unit -> t
val tileset : t -> (int * string) option
val object_ : t -> Object0.t
