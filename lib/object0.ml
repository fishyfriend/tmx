module Text = struct
  module Halign = struct
    type t = [`Center | `Right | `Justify | `Left] [@@deriving eq, ord, show]
  end

  type halign = Halign.t

  module Valign = struct
    type t = [`Center | `Bottom | `Top] [@@deriving eq, ord, show]
  end

  type valign = Valign.t

  type t =
    { text : string; [@main]
      fontfamily : string option;
      pixelsize : int option;
      wrap : bool option;
      color : Color.t option;
      bold : bool option;
      italic : bool option;
      underline : bool option;
      strikeout : bool option;
      kerning : bool option;
      halign : Halign.t option;
      valign : Valign.t option }
  [@@deriving eq, ord, show, make]

  let text t = t.text
  let fontfamily t = Option.value t.fontfamily ~default:"sans-serif"
  let pixelsize t = Option.value t.pixelsize ~default:16
  let wrap t = Option.value t.wrap ~default:false
  let color t = Option.value t.color ~default:Color.black
  let bold t = Option.value t.bold ~default:false
  let italic t = Option.value t.italic ~default:false
  let underline t = Option.value t.underline ~default:false
  let strikeout t = Option.value t.strikeout ~default:false
  let kerning t = Option.value t.kerning ~default:true
  let halign t = Option.value t.halign ~default:`Left
  let valign t = Option.value t.valign ~default:`Top
end

type text = Text.t

module Shape = struct
  type t =
    [ `Rectangle
    | `Ellipse
    | `Point
    | `Polygon of (float * float) list
    | `Polyline of (float * float) list
    | `Text of Text.t
    | `Tile of Gid.t ]
  [@@deriving eq, ord, show]
end

type shape = Shape.t

type t =
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
    properties : Property0.t list;
    shape : Shape.t option }
[@@deriving eq, ord, show, make]

let id t = Option.value t.id ~default:0
let name t = Option.value t.name ~default:""
let class_ t = Option.value t.class_ ~default:""
let x t = Option.value t.x ~default:0.
let y t = Option.value t.y ~default:0.
let rotation t = Option.value t.rotation ~default:0.
let visible t = Option.value t.visible ~default:true
let template t = t.template
let properties t = t.properties
let shape t = Option.value t.shape ~default:`Rectangle

let width t =
  match shape t with
  | `Point -> 0.
  | `Polygon pts | `Polyline pts ->
      let xmin, xmax =
        List.fold_left
          (fun (xmin, xmax) (x, _) -> (min xmin x, max xmax x))
          (0., 0.) pts in
      abs_float (xmax -. xmin)
  | _ -> 0.

let height t =
  match shape t with
  | `Point -> 0.
  | `Polygon pts | `Polyline pts ->
      let ymin, ymax =
        List.fold_left
          (fun (ymin, ymax) (_, y) -> (min ymin y, max ymax y))
          (0., 0.) pts in
      abs_float (ymax -. ymin)
  | _ -> 0.
