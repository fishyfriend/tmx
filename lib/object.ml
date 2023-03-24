include Object_intf

open Util.Option_infix

module Make (State : State_intf.S) : S = struct
  include Object0

  let template_class t =
    template t >>= State.get_template >|= Template0.object_ >>= Object0.class_

  let template_properties t =
    template t >>= State.get_template >|= Template0.object_
    >|= Object0.properties |? []

  (* TODO: I have added [Object0.raw_shape] temporarily to provide direct
     access to the shape record field. Possibly *all* "0" types should just be
     bare records. Put the nice accessors in the "non-0" modules. This would
     simplify the code structure a lot. We might be able to put record types in
     the *_intf modules and avoid the need for "0" types entirely.
     Although...the accessors are certainly handy. *)

  let shape t =
    let sh =
      match Object0.raw_shape t with
      | Some _ as sh -> sh
      | None ->
          Object0.template t >>= State.get_template >|= Template0.object_
          >>= Object0.raw_shape in
    sh |? `Rectangle

  let tile t =
    match shape t with `Tile gid -> State.get_object_tile t gid | _ -> None

  let tile_class t = tile t >>= Tile0.class_

  let tile_properties t = tile t >|= Tile0.properties |? []

  let class_ t =
    match Object0.class_ t with
    | Some _ as c -> c
    | None ->
      (match template_class t with Some _ as c -> c | None -> tile_class t)

  let class_properties t =
    class_ t >>= State.get_class ~useas:`Object >|= Class0.members |? []

  include Properties.Make0 (struct
    type t = Object0.t

    let property_lists t =
      [ class_properties t; tile_properties t; template_properties t;
        properties t ]
  end)
end
