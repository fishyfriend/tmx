module Format = struct
  type t = [`Bmp | `Gif | `Jpg | `Png] [@@deriving eq, ord, show]
end

type format = Format.t

module Source = struct
  type t = [`File of string | `Embed of Format.t * Data.t]
  [@@deriving eq, ord, show]
end

type source = Source.t

type t =
  { source : Source.t;
    trans : Color.t option;
    width : int option;
    height : int option }
[@@deriving eq, ord, show, make]

let source t = t.source
let trans t = t.trans
let width t = t.width
let height t = t.height
