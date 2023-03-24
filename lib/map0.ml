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
    infinite : bool option;
    properties : Property0.t list;
    tilesets : (int * string) list;
    layers : Layer0.t list;
    variant : Variant.t }
[@@deriving eq, ord, show]

let make ~version ?tiledversion ?class_ ?renderorder ?compressionlevel ~width
    ~height ~tilewidth ~tileheight ?parallaxoriginx ?parallaxoriginy
    ?backgroundcolor ?infinite ?(properties = []) ?(tilesets = [])
    ?(layers = []) ~variant () =
  let layers =
    let cmp l l' = Int.compare (Layer0.id l) (Layer0.id l') in
    let layers' = List.sort_uniq cmp layers in
    if List.compare_lengths layers layers' = 0 then layers'
    else Util.invalid_arg "layers" "id not unique" in
  let tilesets =
    let tilesets' =
      let cmp (gid, _) (gid', _) = Int.compare gid gid' in
      List.sort_uniq cmp tilesets in
    if List.compare_lengths tilesets tilesets' = 0 then tilesets'
    else Util.invalid_arg "tilesets" "firstgid not unique" in
  { version;
    tiledversion;
    class_;
    renderorder;
    compressionlevel;
    width;
    height;
    tilewidth;
    tileheight;
    parallaxoriginx;
    parallaxoriginy;
    backgroundcolor;
    infinite;
    properties;
    tilesets;
    layers;
    variant }

let version t = t.version
let tiledversion t = t.tiledversion
let class_ t = t.class_
let renderorder t = t.renderorder |? `Right_down
let compressionlevel t = t.compressionlevel |? -1
let width t = t.width
let height t = t.height
let tilewidth t = t.tilewidth
let tileheight t = t.tileheight
let parallaxoriginx t = t.parallaxoriginx |? 0
let parallaxoriginy t = t.parallaxoriginy |? 0
let backgroundcolor t = t.backgroundcolor |? Color.trans
let infinite t = t.infinite |? false
let properties t = t.properties
let tilesets t = t.tilesets
let layers t = t.layers
let variant t = t.variant

let set_layers t layers = {t with layers}

let objects t = List.concat_map Layer0.objects (layers t)
let get_object t id = List.find_opt (fun o -> Object0.id o = id) (objects t)
let get_object_exn t id =
  match get_object t id with Some o -> o | None -> Util.object_not_found id

let nextlayerid t =
  List.fold_left (fun id l -> max id (Layer0.id l + 1)) 0 (layers t)

let nextobjectid t =
  List.fold_left
    (fun id l ->
      List.fold_left
        (fun id' o -> max id' (Object0.id o + 1))
        id (Layer0.objects l) )
    0 (layers t)
