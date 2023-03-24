open Util.Option_infix

module Map = Stdlib.Map.Make (Int)

module Tileoffset = struct
  type t = {x : int option; y : int option} [@@deriving eq, ord, show, make]

  let x t = t.x |? 0
  let y t = t.y |? 0
end

type tileoffset = Tileoffset.t

module Single = struct
  type t =
    { tilecount : int;
      tilewidth : int;
      tileheight : int;
      spacing : int option;
      margin : int option;
      image : Image.t [@main] }
  [@@deriving eq, ord, show, make]

  let tilecount t = t.tilecount
  let tilewidth t = t.tilewidth
  let tileheight t = t.tileheight
  let spacing t = t.spacing |? 0
  let margin t = t.margin |? 0
  let image t = t.image
end

type single = Single.t

module Objectalignment = struct
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

module Tilerendersize = struct
  type t = [`Tile | `Grid] [@@deriving eq, ord, show]
end

type tilerendersize = Tilerendersize.t

module Fillmode = struct
  type t = [`Stretch | `Preserve_aspect_fit] [@@deriving eq, ord, show]
end

type fillmode = Fillmode.t

module Grid = struct
  type t = [`Orthogonal | `Isometric of int * int] [@@deriving eq, ord, show]
end

type grid = Grid.t

module Variant = struct
  type t = [`Single of Single.t | `Collection] [@@deriving eq, ord, show]
end

type variant = Variant.t

type t =
  { name : string;
    class_ : string option;
    columns : int;
    objectalignment : Objectalignment.t option;
    tilerendersize : Tilerendersize.t option;
    fillmode : Fillmode.t option;
    tileoffset : Tileoffset.t option;
    grid : Grid.t option;
    properties : Property0.t list;
    tiles : Tile0.t Map.t; [@opaque] [@main]
    variant : Variant.t }
[@@deriving eq, ord, show]

let make ~name ?class_ ~columns ?objectalignment ?tilerendersize ?fillmode
    ?tileoffset ?grid ?(properties = []) ~variant tiles =
  let tile_map_of_list tiles =
    List.to_seq tiles
    |> Seq.map (fun tile -> (Tile0.id tile, tile))
    |> Map.of_seq in
  let tiles =
    match variant with
    | `Single _ -> tile_map_of_list tiles
    | `Collection -> tile_map_of_list tiles in
  { name;
    class_;
    columns;
    objectalignment;
    tilerendersize;
    fillmode;
    tileoffset;
    grid;
    properties;
    tiles;
    variant }

let name t = t.name
let class_ t = t.class_
let columns t = t.columns
let grid t = t.grid |? `Orthogonal
let tilerendersize t = t.tilerendersize |? `Tile
let fillmode t = t.fillmode |? `Stretch
let tileoffset t = t.tileoffset
let properties t = t.properties
let variant t = t.variant
let tiles t = Map.bindings t.tiles |> List.map snd

let objectalignment t =
  match (t.objectalignment, grid t) with
  | Some x, _ -> x
  | None, `Orthogonal -> `Bottomleft
  | None, `Isometric _ -> `Bottom

let tilecount t =
  match variant t with
  | `Single single -> Single.tilecount single
  | `Collection -> Map.cardinal t.tiles

let max_id t =
  match variant t with
  | `Single _ -> tilecount t - 1
  | `Collection -> Map.fold (fun id _ max_id -> max id max_id) t.tiles 0

let get_tile t id : Tile0.t option =
  match variant t with
  | `Collection -> Map.find_opt id t.tiles
  | `Single single ->
      if id >= tilecount t then None
      else
        let tile =
          match Map.find_opt id t.tiles with
          | Some tile -> tile
          | None -> Tile0.make ~id () in
        let image = Single.image single in
        let width = Single.tilewidth single in
        let height = Single.tileheight single in
        let margin = Single.margin single in
        let spacing = Single.spacing single in
        let columns = columns t in
        let col = id mod columns in
        let row = id / columns in
        let x = margin + (col * (width + spacing)) in
        let y = margin + (row * (height + spacing)) in
        Some tile
        >|= Fun.flip Tile0.set_image (Some image)
        >|= Fun.flip Tile0.set_x (Some x)
        >|= Fun.flip Tile0.set_y (Some y)
        >|= Fun.flip Tile0.set_width (Some width)
        >|= Fun.flip Tile0.set_height (Some height)
