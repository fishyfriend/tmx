open Util.Option.Infix

module type S = sig
  include module type of Basic

  val run_context : ('a, Context.t) State.t -> 'a
  val read_context : (Context.t -> 'a) -> 'a
end

module Make () : S = struct
  let the_context = ref Context.default

  let run_context state =
    let result, context = State.run state !the_context in
    the_context := context ;
    result

  let read_context state = run_context (State.read state)

  module type ClassPropsT = Sigs.ClassPropsT with type property := Property0.t
  type 'a class_props = (module ClassPropsT with type t = 'a)

  let make_std_plists (type a) ((module T) : a class_props) ~useas (t : a) =
    let class_props =
      T.class_ t
      >>= (fun c -> read_context (Context.get_class ~useas c))
      >|= Basic.Class.members |? [] in
    let own_props = T.properties t in
    [class_props; own_props]

  let make_std_props (type a) ((module T) : a class_props) ~useas : a Props.t =
    Props.make_deep ~strict:false @@ make_std_plists (module T) ~useas

  module Property = struct
    include Basic.Property
    let plists t = make_std_plists (module Basic.Property) ~useas:`Property t
    include (val Props.make_deep ~strict:true plists)
  end

  module Object = struct
    include Basic.Object

    let template_object t =
      template t
      >>= (fun tem -> read_context (Context.get_template tem))
      >|= Basic.Template.object_

    let shape t =
      match shape t with
      | `Rectangle -> template_object t >|= shape |? `Rectangle
      | sh -> sh

    let tile t =
      match shape t with
      | `Tile gid -> read_context (Context.get_object_tile t gid)
      | _ -> None

    let tile_class t = tile t >>= Basic.Tile.class_

    let class_ t =
      match class_ t with
      | Some _ as c -> c
      | None ->
        ( match template_object t >>= class_ with
        | Some _ as c -> c
        | None -> tile_class t )

    let class_properties t =
      class_ t
      >>= (fun c -> read_context (Context.get_class ~useas:`Object c))
      >|= Basic.Class.members |? []

    let template_properties t = template_object t >|= properties |? []

    let tile_properties t = tile t >|= Basic.Tile.properties |? []

    let plists t =
      [ class_properties t; tile_properties t; template_properties t;
        properties t ]

    include (val Props.make_deep ~strict:false plists)
  end

  module Layer = struct
    include Basic.Layer
    include (val make_std_props (module Basic.Layer) ~useas:`Layer)
  end

  module Tile = struct
    include Basic.Tile
    include (val make_std_props (module Basic.Tile) ~useas:`Tile)
  end

  module Tileset = struct
    include Basic.Tileset
    include (val make_std_props (module Basic.Tileset) ~useas:`Tileset)
  end

  module Map = struct
    include Basic.Map
    include (val make_std_props (module Basic.Map) ~useas:`Map)
  end

  module Template = Basic.Template
  module Class = Basic.Class
  module Enum = Basic.Enum
  module Customtype = Basic.Customtype
end
