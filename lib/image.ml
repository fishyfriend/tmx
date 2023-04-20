module Format = struct
  type t = [`Bmp | `Gif | `Jpg | `Png]
  [@@deriving eq, ord, show {with_path = false}]
end

type format = Format.t

module Source = struct
  type t = [`File of string | `Embed of Format.t * Data.t]
  [@@deriving eq, ord, show {with_path = false}]

  let reloc t ~from_ ~to_ =
    match t with
    | `File fname -> `File (Util.Filename.reloc ~from_ ~to_ fname)
    | _ -> t
end

type source = Source.t

type t =
  { source : Source.t;
    trans : Color.t option;
    width : int option;
    height : int option }
[@@deriving eq, ord, show {with_path = false}, make]

let source t = t.source
let trans t = t.trans
let width t = t.width
let height t = t.height

let reloc t ~from_ ~to_ = {t with source = Source.reloc t.source ~from_ ~to_}
