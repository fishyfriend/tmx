open Util.Option_infix

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
  let fontfamily t = t.fontfamily |? "sans-serif"
  let pixelsize t = t.pixelsize |? 16
  let wrap t = t.wrap |? false
  let color t = t.color |? Color.black
  let bold t = t.bold |? false
  let italic t = t.italic |? false
  let underline t = t.underline |? false
  let strikeout t = t.strikeout |? false
  let kerning t = t.kerning |? true
  let halign t = t.halign |? `Left
  let valign t = t.valign |? `Top
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

let id t = t.id |? 0
let name t = t.name
let class_ t = t.class_
let x t = t.x |? 0.
let y t = t.y |? 0.
let rotation t = t.rotation |? 0.
let visible t = t.visible |? true
let template t = t.template
let properties t = t.properties
let shape t = t.shape |? `Rectangle

let set_shape t shape = {t with shape}

(* TODO: this will go away? *)
let raw_shape t = t.shape

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
