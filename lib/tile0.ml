module Frame = struct
  type t = {tileid : int; duration : int} [@@deriving eq, ord, show, make]

  let tileid t = t.tileid
  let duration t = t.duration
end

type frame = Frame.t

type t =
  { id : int;
    class_ : string option;
    x : int option;
    y : int option;
    width : int option;
    height : int option;
    properties : Property0.t list;
    image : Image.t option;
    objectgroup : Object0.t list;
    animation : Frame.t list }
[@@deriving eq, ord, show, make]

let id t = t.id
let class_ t = Option.value t.class_ ~default:""
let x t = Option.value t.x ~default:0
let y t = Option.value t.y ~default:0
let image t = t.image
let properties t = t.properties
let objectgroup t = t.objectgroup
let animation t = t.animation

let width t =
  match (t.width, image t) with None, Some img -> Image.width img | w, _ -> w

let height t =
  match (t.height, image t) with
  | None, Some img -> Image.height img
  | h, _ -> h

let set_image t image = {t with image}
let set_x t x = {t with x}
let set_y t y = {t with y}
let set_width t width = {t with width}
let set_height t height = {t with height}
