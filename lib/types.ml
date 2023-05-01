module Int_map = Map.Make (Int)

type property =
  { name : string;
    propertytype : string option;
    value :
      [ `String of string
      | `Int of int
      | `Float of float
      | `Bool of bool
      | `Color of Color.t
      | `File of string
      | `Object of int
      | `Class of property list ] }

type data =
  { encoding : [`Base64 | `Csv] option;
    compression : [`Gzip | `Zlib | `Zstd] option;
    bytes : bytes }

type image =
  { source : [`File of string | `Embed of [`Bmp | `Gif | `Jpg | `Png] * data];
    trans : Color.t option;
    width : int option;
    height : int option }

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
    halign : [`Center | `Right | `Justify | `Left] option;
    valign : [`Center | `Bottom | `Top] option }

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
    shape :
      [ `Rectangle
      | `Ellipse
      | `Point
      | `Polygon of (float * float) list
      | `Polyline of (float * float) list
      | `Text of text
      | `Tile of Gid.t ]
      option }

type tilelayer = {width : int; height : int; data : data}

type objectgroup =
  {draworder : [`Topdown | `Index] option; objects : object_ Int_map.t}

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

type tileoffset = {x : int option; y : int option}

type single =
  { tilecount : int;
    tilewidth : int;
    tileheight : int;
    spacing : int option;
    margin : int option;
    image : image }

type tileset =
  { name : string;
    class_ : string option;
    columns : int;
    objectalignment :
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
      option;
    tilerendersize : [`Tile | `Grid] option;
    fillmode : [`Stretch | `Preserve_aspect_fit] option;
    tileoffset : tileoffset option;
    grid : [`Orthogonal | `Isometric of int * int] option;
    properties : property list;
    tiles : tile Int_map.t;
    variant : [`Single of single | `Collection] }

type staggered = {staggeraxis : [`X | `Y]; staggerindex : [`Even | `Odd]}

type hexagonal =
  {hexsidelength : int; staggeraxis : [`X | `Y]; staggerindex : [`Even | `Odd]}

type geometry =
  [`Orthogonal | `Isometric | `Staggered of staggered | `Hexagonal of hexagonal]

type map =
  { version : int * int;
    tiledversion : string option;
    class_ : string option;
    renderorder : [`Right_down | `Right_up | `Left_down | `Left_up] option;
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

type enum =
  {storagetype : [`Int | `String]; valuesasflags : bool; values : string array}

type customtype =
  {id : int; name : string; variant : [`Class of class_ | `Enum of enum]}
