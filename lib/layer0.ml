open Util.Option_infix

module Tilelayer = struct
  type t = {width : int; height : int; data : Data.t option}
  [@@deriving eq, ord, show, make]

  let width t = t.width
  let height t = t.height
  let data t = t.data
end

type tilelayer = Tilelayer.t

module Objectgroup = struct
  module Draworder = struct
    type t = [`Topdown | `Index] [@@deriving eq, ord, show]
  end

  type draworder = Draworder.t

  type t = {draworder : Draworder.t option; objects : Object0.t list}
  [@@deriving eq, ord, show, make]

  let draworder t = t.draworder |? `Topdown
  let objects t = t.objects
  let get_object t id = List.find_opt (fun o -> Object0.id o = id) (objects t)
  let get_object_exn t id =
    match get_object t id with Some o -> o | None -> Util.object_not_found id
end

type objectgroup = Objectgroup.t

module Imagelayer = struct
  type t =
    {image : Image.t option; repeatx : bool option; repeaty : bool option}
  [@@deriving eq, ord, show, make]

  let image t = t.image
  let repeatx t = t.repeatx |? false
  let repeaty t = t.repeaty |? false
end

type imagelayer = Imagelayer.t

module Layer = struct
  type t =
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
      properties : Property0.t list;
      variant :
        [ `Tilelayer of Tilelayer.t
        | `Objectgroup of Objectgroup.t
        | `Imagelayer of Imagelayer.t
        | `Group of t list ] }
  [@@deriving eq, ord, show, make]
end

include Layer

module Variant = struct
  type t =
    [ `Tilelayer of Tilelayer.t
    | `Objectgroup of Objectgroup.t
    | `Imagelayer of Imagelayer.t
    | `Group of Layer.t list ]
  [@@deriving eq, ord, show]
end

type variant = Variant.t

let id t = t.id |? 0
let name t = t.name |? ""
let class_ t = t.class_
let opacity t = t.opacity |? 1.
let visible t = t.visible |? true
let tintcolor t = t.tintcolor
let offsetx t = t.offsetx |? 0.
let offsety t = t.offsety |? 0.
let parallaxx t = t.parallaxx |? 1.
let parallaxy t = t.parallaxy |? 1.
let properties t = t.properties
let variant t = t.variant

let rec objects t =
  match t.variant with
  | `Objectgroup og -> Objectgroup.objects og
  | `Group ts -> List.concat_map objects ts
  | _ -> []

let rec get_object t id =
  match t.variant with
  | `Objectgroup og -> Objectgroup.get_object og id
  | `Group ts -> List.find_map (fun t -> get_object t id) ts
  | _ -> None

let get_object_exn t id =
  match get_object t id with Some o -> o | None -> Util.object_not_found id
