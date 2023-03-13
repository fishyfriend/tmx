module Tileoffset : sig
  type t [@@deriving eq, ord, show]

  val make : ?x:int -> ?y:int -> unit -> t
  val x : t -> int
  val y : t -> int
end

type tileoffset = Tileoffset.t

module Single : sig
  type t [@@deriving eq, ord, show]

  val make :
    tilecount:int ->
    tilewidth:int ->
    tileheight:int ->
    ?spacing:int ->
    ?margin:int ->
    Image.t ->
    t

  val tilecount : t -> int
  val tilewidth : t -> int
  val tileheight : t -> int
  val spacing : t -> int
  val margin : t -> int
  val image : t -> Image.t
end

type single = Single.t

module Objectalignment : sig
  type t =
    [ `Unspecified
    | `Topleft
    | `Top
    | `Topright
    | `Left
    | `Center
    | `Right
    | `Bottomleft
    | `Bottom
    | `Bottomright ]
  [@@deriving eq, ord, show]
end

type objectalignment = Objectalignment.t

module Tilerendersize : sig
  type t = [`Tile | `Grid] [@@deriving eq, ord, show]
end

type tilerendersize = Tilerendersize.t

module Fillmode : sig
  type t = [`Stretch | `Preserve_aspect_fit] [@@deriving eq, ord, show]
end

type fillmode = Fillmode.t

module Grid : sig
  type t = [`Orthogonal | `Isometric of int * int] [@@deriving eq, ord, show]
end

type grid = Grid.t

module Variant : sig
  type t = [`Single of Single.t | `Collection] [@@deriving eq, ord, show]
end

type variant = Variant.t

type t [@@deriving eq, ord, show]

val make :
  name:string ->
  ?class_:string ->
  columns:int ->
  ?objectalignment:Objectalignment.t ->
  ?tilerendersize:Tilerendersize.t ->
  ?fillmode:Fillmode.t ->
  ?tileoffset:Tileoffset.t ->
  ?grid:Grid.t ->
  ?properties:Property0.t list ->
  variant:variant ->
  Tile0.t list ->
  t

val name : t -> string
val class_ : t -> string
val tilecount : t -> int
val columns : t -> int
val objectalignment : t -> Objectalignment.t
val tilerendersize : t -> Tilerendersize.t
val fillmode : t -> Fillmode.t
val tileoffset : t -> Tileoffset.t option
val grid : t -> Grid.t
val properties : t -> Property0.t list
val variant : t -> variant
val tiles : t -> Tile0.t list
val get_tile : t -> int -> Tile0.t option
