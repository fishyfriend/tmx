open Util.Option_infix

module Staggeraxis = struct type t = [`X | `Y] [@@deriving eq, ord, show] end

type staggeraxis = Staggeraxis.t

module Staggerindex = struct
  type t = [`Even | `Odd] [@@deriving eq, ord, show]
end

type staggerindex = Staggerindex.t

module Staggered = struct
  type t = {staggeraxis : Staggeraxis.t; staggerindex : Staggerindex.t}
  [@@deriving eq, ord, show, make]

  let staggeraxis t = t.staggeraxis
  let staggerindex t = t.staggerindex
end

type staggered = Staggered.t

module Hexagonal = struct
  type t =
    { hexsidelength : int;
      staggeraxis : Staggeraxis.t;
      staggerindex : Staggerindex.t }
  [@@deriving eq, ord, show, make]

  let hexsidelength t = t.hexsidelength
  let staggeraxis t = t.staggeraxis
  let staggerindex t = t.staggerindex
end

type hexagonal = Hexagonal.t

module Renderorder = struct
  type t = [`Right_down | `Right_up | `Left_down | `Left_up]
  [@@deriving eq, ord, show]
end

type renderorder = Renderorder.t

module Variant = struct
  type t =
    [ `Orthogonal
    | `Isometric
    | `Staggered of Staggered.t
    | `Hexagonal of Hexagonal.t ]
  [@@deriving eq, ord, show]
end

type variant = Variant.t

type t =
  { version : string;
    tiledversion : string option;
    class_ : string option;
    renderorder : Renderorder.t option;
    compressionlevel : int option;
    width : int;
    height : int;
    tilewidth : int;
    tileheight : int;
    parallaxoriginx : int option;
    parallaxoriginy : int option;
    backgroundcolor : Color.t option;
    nextlayerid : int option;
    nextobjectid : int option;
    infinite : bool option;
    properties : Property0.t list;
    tilesets : (int * [`File of string | `Embed of Tileset0.t]) list;
    layers : Layer0.t list;
    variant : Variant.t }
[@@deriving eq, ord, show, make]

let version t = t.version
let tiledversion t = t.tiledversion
let class_ t = t.class_ |? ""
let renderorder t = t.renderorder |? `Right_down
let compressionlevel t = t.compressionlevel |? -1
let width t = t.width
let height t = t.height
let tilewidth t = t.tilewidth
let tileheight t = t.tileheight
let parallaxoriginx t = t.parallaxoriginx |? 0
let parallaxoriginy t = t.parallaxoriginy |? 0
let backgroundcolor t = t.backgroundcolor |? Color.trans
let nextlayerid t = t.nextlayerid |? 0 (* TODO *)
let nextobjectid t = t.nextobjectid |? 0 (* TODO *)
let infinite t = t.infinite |? false
let properties t = t.properties
let tilesets t = t.tilesets
let layers t = t.layers
let variant t = t.variant

let objects t = List.concat_map Layer0.objects (layers t)
let get_object t id = List.find_opt (fun o -> Object0.id o = id) (objects t)
let get_object_exn t id =
  match get_object t id with Some o -> o | None -> Util.object_not_found id
