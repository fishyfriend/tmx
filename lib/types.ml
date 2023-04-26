module Int_map = Map.Make (Int)

type format = [`Bmp | `Gif | `Jpg | `Png]

type source = [`File of string | `Embed of format * Data.t]

type image =
  { source : source;
    trans : Color.t option;
    width : int option;
    height : int option }

type property = {name : string; propertytype : string option; value : value}

and value =
  [ `String of string
  | `Int of int
  | `Float of float
  | `Bool of bool
  | `Color of Color.t
  | `File of string
  | `Object of int
  | `Class of property list ]

type halign = [`Center | `Right | `Justify | `Left]

type valign = [`Center | `Bottom | `Top]

type text =
  { text : string;
    fontfamily : string option;
    pixelsize : int option;
    wrap : bool option;
    color : Color.t option;
    bold : bool option;
    italic : bool option;
    underline : bool option;
    strikeout : bool option;
    kerning : bool option;
    halign : halign option;
    valign : valign option }

type shape =
  [ `Rectangle
  | `Ellipse
  | `Point
  | `Polygon of (float * float) list
  | `Polyline of (float * float) list
  | `Text of text
  | `Tile of Gid.t ]

type object_ =
  { id : int option;
    name : string option;
    class_ : string option;
    x : float option;
    y : float option;
    width : float option;
    height : float option;
    rotation : float option;
    visible : bool option;
    template : string option;
    properties : property list;
    shape : shape option }

type tilelayer = {width : int; height : int; data : Data.t}

type draworder = [`Topdown | `Index]

type objectgroup = {draworder : draworder option; objects : object_ Int_map.t}

type imagelayer =
  {image : image option; repeatx : bool option; repeaty : bool option}

type layer =
  { id : int option;
    name : string option;
    class_ : string option;
    opacity : float option;
    visible : bool option;
    tintcolor : Color.t option;
    offsetx : float option;
    offsety : float option;
    parallaxx : float option;
    parallaxy : float option;
    properties : property list;
    variant :
      [ `Tilelayer of tilelayer
      | `Objectgroup of objectgroup
      | `Imagelayer of imagelayer
      | `Group of layer list ] }

type layer_variant =
  [ `Tilelayer of tilelayer
  | `Objectgroup of objectgroup
  | `Imagelayer of imagelayer
  | `Group of layer list ]

type frame = {tileid : int; duration : int}

type tile =
  { id : int;
    class_ : string option;
    x : int option;
    y : int option;
    width : int option;
    height : int option;
    properties : property list;
    image : image option;
    objectgroup : object_ list;
    animation : frame list }

type 'a int_map = 'a Stdlib.Map.Make(Int).t

type tileoffset = {x : int option; y : int option}

type single =
  { tilecount : int;
    tilewidth : int;
    tileheight : int;
    spacing : int option;
    margin : int option;
    image : image }

type objectalignment =
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

type tilerendersize = [`Tile | `Grid]

type fillmode = [`Stretch | `Preserve_aspect_fit]

type grid = [`Orthogonal | `Isometric of int * int]

type tileset_variant = [`Single of single | `Collection]

type tileset =
  { name : string;
    class_ : string option;
    columns : int;
    objectalignment : objectalignment option;
    tilerendersize : tilerendersize option;
    fillmode : fillmode option;
    tileoffset : tileoffset option;
    grid : grid option;
    properties : property list;
    tiles : tile Int_map.t;
    variant : tileset_variant }

type staggeraxis = [`X | `Y]

type staggerindex = [`Even | `Odd]

type staggered = {staggeraxis : staggeraxis; staggerindex : staggerindex}

type hexagonal =
  {hexsidelength : int; staggeraxis : staggeraxis; staggerindex : staggerindex}

type renderorder = [`Right_down | `Right_up | `Left_down | `Left_up]

type geometry =
  [`Orthogonal | `Isometric | `Staggered of staggered | `Hexagonal of hexagonal]

type map =
  { version : string;
    tiledversion : string option;
    class_ : string option;
    renderorder : renderorder option;
    compressionlevel : int option;
    width : int;
    height : int;
    tilewidth : int;
    tileheight : int;
    parallaxoriginx : int option;
    parallaxoriginy : int option;
    backgroundcolor : Color.t option;
    infinite : bool option;
    properties : property list;
    tilesets : (int * string) list;
    layers : layer list;
    geometry : geometry }

type template = {tileset : (int * string) option; object_ : object_}

type useas =
  [ `Property
  | `Map
  | `Layer
  | `Object
  | `Tile
  | `Tileset
  | `Wangcolor
  | `Wangset ]

type class_ = {useas : useas list; members : property list}

type storagetype = [`Int | `String]

type customtype_variant = [`Class of class_ | `Enum of Enum.t]

type customtype = {id : int; name : string; variant : customtype_variant}
