open Util.Option.Infix

module Make () = struct
  let the_context = ref Context.empty

  let run_context state =
    let result, context = State.run state !the_context in
    the_context := context ;
    result

  module type Properties =
    Sigs.Properties0 with type property := Basic.Property.t

  module type PlistsT = sig
    type t

    val property_lists : t -> Basic.Property.t list list
  end

  module type ClassT = sig
    type t
    val useas : Basic.Class.useas
    val class_ : t -> string option
    val properties : t -> Basic.Property.t list
  end

  let rec merge_propertys t0 t =
    let name = Basic.Property.name t in
    let propertytype =
      match Basic.Property.propertytype t with
      | Some _ as pt -> pt
      | None -> Basic.Property.propertytype t0 in
    let value =
      match Basic.Property.(value t0, value t) with
      (* Promote raw JSON types as needed. *)
      | `Int _, `Float x -> `Int (int_of_float x)
      | `Color _, `String s -> `Color (Color.of_string s)
      | `File _, `String s -> `File s
      | `Object _, `Float x -> `Object (int_of_float x)
      | `Class ts0, `Class ts ->
          `Class (merge_property_lists ~strict:false ts0 ts)
      | _, v -> v in
    Basic.Property.make ~name ?propertytype ~value ()

  and merge_property_lists ~strict ts0 ts =
    match (ts0, ts) with
    | _, [] -> ts0
    | [], _ -> if strict then [] else ts
    | t0 :: ts0', t :: ts' ->
      ( match
          String.compare (Basic.Property.name t0) (Basic.Property.name t)
        with
      | -1 -> t0 :: merge_property_lists ~strict ts0' ts
      | 1 ->
          if strict then merge_property_lists ~strict ts0 ts'
          else t :: merge_property_lists ~strict ts0 ts'
      | 0 -> merge_propertys t0 t :: merge_property_lists ~strict ts0' ts'
      | _ -> assert false )

  let make_props00 ~strict (type a) (module T : PlistsT with type t = a) :
      (module Properties with type t = a) =
    ( module struct
      type t = a

      let properties t =
        List.fold_left (merge_property_lists ~strict) [] (T.property_lists t)

      (* TODO: It should be possible to rewrite [properties] and [get_property]
         so as to reuse the traversal/filtration logic *)
      let get_property k t =
        let find k ps = List.find_opt (fun p -> Basic.Property.name p = k) ps in
        match T.property_lists t with
        | [] -> None
        | plist :: plists ->
          ( match (strict, find k plist) with
          | true, None -> None
          | _, p ->
              List.fold_left
                (fun p plist ->
                  match (p, find k plist) with
                  | p', None | None, p' -> p'
                  | Some p, Some p' -> Some (merge_propertys p p') )
                p plists )

      let get_property_exn k t =
        match get_property k t with
        | Some p -> p
        | None -> Util.not_found "property" k
    end )

  module Make_props0 (T : PlistsT) : Properties with type t := T.t =
    (val make_props00 ~strict:false (module T))

  module Make_props0_strict (T : PlistsT) : Properties with type t := T.t =
    (val make_props00 ~strict:true (module T))

  module MakePlistsT (T : ClassT) : PlistsT with type t = T.t = struct
    include T

    let property_lists t =
      let class_props =
        class_ t
        >>= Fun.flip Context.get_class !the_context ~useas
        >|= Basic.Class.members |? [] in
      let own_props = properties t in
      [class_props; own_props]
  end

  module Make (T : ClassT) : Properties with type t := T.t =
    Make_props0 (MakePlistsT (T))

  module Make_strict (T : ClassT) : Properties with type t := T.t =
    Make_props0_strict (MakePlistsT (T))

  module Property = struct
    include Basic.Property

    include Make_strict (struct
      type nonrec t = t

      let useas = `Property

      let class_ t =
        match value t with `Class _ -> propertytype t | _ -> None

      let properties t = match value t with `Class props -> props | _ -> []
    end)
  end
  module Object = struct
    include Basic.Object

    let template_class t =
      template t
      >>= Fun.flip Context.get_template !the_context
      >|= Basic.Template.object_ >>= Basic.Object.class_

    let template_properties t =
      template t
      >>= Fun.flip Context.get_template !the_context
      >|= Basic.Template.object_ >|= properties |? []

    (* TODO: I have added [Object0.raw_shape] temporarily to provide direct
       access to the shape record field. Possibly *all* "0" types should just
       be bare records. Put the nice accessors in the "non-0" modules. This
       would simplify the code structure a lot. We might be able to put record
       types in the *_intf modules and avoid the need for "0" types entirely.
       Although...the accessors are certainly handy. *)

    let shape t =
      let sh =
        match raw_shape t with
        | Some _ as sh -> sh
        | None ->
            template t
            >>= Fun.flip Context.get_template !the_context
            >|= Basic.Template.object_ >>= raw_shape in
      sh |? `Rectangle

    let tile t =
      match shape t with
      | `Tile gid -> Context.get_object_tile t gid !the_context
      | _ -> None

    let tile_class t = tile t >>= Basic.Tile.class_

    let tile_properties t = tile t >|= Basic.Tile.properties |? []

    let class_ t =
      match class_ t with
      | Some _ as c -> c
      | None ->
        (match template_class t with Some _ as c -> c | None -> tile_class t)

    let class_properties t =
      class_ t
      >>= Fun.flip Context.get_class !the_context ~useas:`Object
      >|= Basic.Class.members |? []

    include Make_props0 (struct
      type nonrec t = t

      let property_lists t =
        [ class_properties t; tile_properties t; template_properties t;
          properties t ]
    end)
  end

  module Layer = struct
    include Basic.Layer
    include Make (struct include Basic.Layer let useas = `Layer end)
  end

  module Tile = struct
    include Basic.Tile
    include Make (struct include Basic.Tile let useas = `Tile end)
  end

  module Tileset = struct
    include Basic.Tileset
    include Make (struct include Basic.Tileset let useas = `Tileset end)
  end

  module Map = struct
    include Basic.Map
    include Make (struct include Basic.Map let useas = `Map end)
  end

  module Template = Basic.Template
  module Class = Basic.Class
  module Enum = Basic.Enum
  module Customtype = Basic.Customtype
end
