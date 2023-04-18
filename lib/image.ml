module Format = struct
  type t = [`Bmp | `Gif | `Jpg | `Png]
  [@@deriving eq, ord, show {with_path = false}]
end

type format = Format.t

module Source = struct
  type t = [`File of string | `Embed of Format.t * Data.t]
  [@@deriving eq, ord, show {with_path = false}]

  let relocate t ~from_dir ~to_dir =
    match t with
    | `File fname -> `File (Util.Filename.relocate ~from_dir ~to_dir fname)
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

let relocate t ~from_dir ~to_dir =
  {t with source = Source.relocate t.source ~from_dir ~to_dir}
