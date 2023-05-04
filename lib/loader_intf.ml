module type S = sig
  (** {1 Core types} *)

  module Core : Core.S

  (** @inline *)
  include module type of struct include Core end

  (** {1 Resource loading}

      Each of these functions loads a resource from the file at the specified
      path or returns the previously-loaded resource associated with that path
      if available. Resources are persisted in the loader state once loaded.

      Path arguments must be relative; they are interpreted relative to the
      loader's root directory.

      Dependencies are loaded automatically, e.g., [load_map_xml "a.tmx"] causes
      any tilesets and templates referenced by [a.tmx] to be loaded as well.
      Files referenced by custom properties are loaded optionally depending on
      the setting used when creating the loader.

      Custom Types files work differently; see {!section:customtypes} below.
  *)

  val load_map_xml : string -> (Map.t, Error.t) result
  val load_map_xml_exn : string -> Map.t
  val load_tileset_xml : string -> (Tileset.t, Error.t) result
  val load_tileset_xml_exn : string -> Tileset.t
  val load_template_xml : string -> (Template.t, Error.t) result
  val load_template_xml_exn : string -> Template.t
  val load_file : string -> (string, Error.t) result
  val load_file_exn : string -> string

  (** Base directory for loading *)
  val root : string

  (** {1 Querying loaded resources}

      These resources are keyed by source filename, {b not} the value of the
      resource's [name] field. *)

  val tilesets : unit -> (string * Tileset.t) list
  val templates : unit -> (string * Template.t) list
  val files : unit -> (string * string) list
  val maps : unit -> (string * Map.t) list

  val get_tileset : string -> Tileset.t option
  val get_template : string -> Template.t option
  val get_map : string -> Map.t option
  val get_file : string -> string option
  val get_tile : Gid.t -> Tile.t option

  val get_tileset_exn : string -> Tileset.t
  val get_template_exn : string -> Template.t
  val get_map_exn : string -> Map.t
  val get_file_exn : string -> string
  val get_tile_exn : Gid.t -> Tile.t

  (** Translate a GID from loader context to map context.

      This allows to recover the original value of a GID before it was remapped
      by the loader.

      [None] is returned if the GID references an unknown tile or if the
      tileset that contains the referenced tile is not a dependency of the
      specified map. *)
  val remap_gid_to_map : Gid.t -> Map.t -> Gid.t option

  val remap_gid_to_map_exn : Gid.t -> Map.t -> Gid.t

  (** See [remap_gid_to_map]. *)
  val remap_gid_to_template : Gid.t -> Template.t -> Gid.t option

  val remap_gid_to_template_exn : Gid.t -> Template.t -> Gid.t

  (** {1 Unloading resources}

      These functions remove resources from the loader, allowing memory to be
      freed.

      {b Use with caution.} At present, dependencies are not checked, so
      e.g. unloading a tileset that is used by a currently-loaded map may
      result in broken tile lookups, and unloading a map does not cause any
      of its tilesets to be unloaded automatically.  *)

  val unload_tileset : string -> unit
  val unload_template : string -> unit
  val unload_file : string -> unit
  val unload_map : string -> unit

  (** {1:customtypes Custom types}

      Custom types work differently from other loadable resources. They are
      never auto-loaded and, once loaded, they are keyed by name as returned by
      {!Customtype.name}.

      Multiple custom types with the same name are permitted so long as their
      {!val:Class.useas} fields don't conflict. In this context, enums are
      treated as classes with [useas] equal to [[`Property]]. Thus, for example,
      the following custom types may coexist...

      {[let class1 = Class.make ~useas:[`Object; `Layer] ~members:[(* ... *)] () in
        let class2 = Class.make ~useas:[`Tile] ~members:[(* ... *)] () in
        let enum1 = Enum.make ~storagetype:`Int ~valuesasflags:true [(* ... *)] in
        [ Customtype.make ~id:1 ~name:"foo" ~variant:(`Class class1) ();
          Customtype.make ~id:2 ~name:"foo" ~variant:(`Class class2) ();
          Customtype.make ~id:3 ~name:"foo" ~variant:(`Enum enum1) () ]
      ]}

      ...whereas the following may not:

      {[let class3 = Class.make ~useas:[`Property] ~members:[(* ... *)] () in
        let enum2 = Enum.make ~storagetype:`Int ~valuesasflags:true [(* ... *)] in
        [ Customtype.make ~id:1 ~name:"foo" ~variant:(`Class class3) ();
          Customtype.make ~id:2 ~name:"foo" ~variant:(`Enum enum2) () ]
      ]}

      Tiled itself does not enforce this restriction, but it is necessary to
      avoid ambiguity. *)

  val import_customtypes_json : string -> (Customtype.t list, Error.t) result
  val import_customtypes_json_exn : string -> Customtype.t list
  val customtypes : unit -> Customtype.t list
  val get_customtypes : string -> Customtype.t list
  val remove_customtypes : string -> unit
  val get_class : string -> useas:Class.useas -> Class.t option
  val get_class_exn : string -> useas:Class.useas -> Class.t
  val remove_class : string -> useas:Class.useas -> unit
  val get_enum : string -> Enum.t option
  val get_enum_exn : string -> Enum.t
  val remove_enum : string -> unit
end

type t = (module S)

module type Intf = sig
  (** The entry point for loading and accessing TMX data.

      Typical usage looks like:

      {[let loader = Loader.make ~root:"path/to/my/game" () in
        let module L : Loader.S = (val loader) in
        let m : L.map = L.load_map_xml_exn "level1.tmx" in
        let open L.Core in (* alternative: let open L in *)
        Map.do_something ();
        (* ... *)
      ]}

      A loader provides a stateful context for loading TMX resource files ({!S})
      and a collection of immutable "core types" ({{!S.map}[map]}) representing
      TMX data types. Each core type is paired with a submodule ({{!S.Core.Map}
      [Map]}) providing accessors and other functions.

      The core types can be brought into scope by opening {{!S.Core}[Core]} or
      by opening the loader directly. The former is usually preferable as it
      avoids cluttering the namespace with unprefixed loader functions.

      {2 Transformations}

      A loader applies two important transformations to loaded TMX data.

      First, it rewrites all paths to be relative to its configured root
      directory. For example, a reference to [../sprite.png] that occurs in
      [a/b/tileset.tsx] is rewritten to [a/sprite.png].

      Second, within each map or template, the loader rewrites tile GIDs to
      point into a table of all loaded tilesets. This means that GIDs are
      globally equivalent across maps. If the original GIDs are needed for some
      reason, they can be recovered using {{!S.remap_gid_to_map}
      [remap_gid_to_map]} et al. Also see {{!S.Core.Map.get_tile_by_orig_gid}
      [Map.get_tile_by_orig_gid]}.

      The transformations are done mainly for implementation reasons.

      {2 Semantics}

      The functions on TMX data types simulate the semantics of the equivalent
      data types in the Tiled desktop application, notably by applying object
      templates and custom property inheritance rules.

      This convenience comes with the caveat that accessor functions may return
      different values after the loader state changes. For example, if loading a
      new Custom Types file would result in new properties being inherited by
      some map object, code like the following might succeed:

      {[(* ... continuing the previous snippet *)
        let o = Map.get_object_exn m 42 in
        assert (Object.get_property "active" o = None);
        let _ = import_customtypes_json_exn "propertytypes.json" in
        assert (Object.get_property "active" o =
                Property.make ~name:"active" ~value:(`Bool true) ())
      ]}

      To avoid surprises, ensure that all required Custom Types are loaded
      before using property values.

      For details on custom property inheritance rules, see
      {{:https://discourse.mapeditor.org/t/interitance-hierarchy-of-custom-properties/2749/3}
      this post} by Tiled's primary developer. (In the post, the older term
      "ObjectType" should be read as equivalent to the current Tiled concept of
      "class.")

      To get a core type's "raw" custom properties without applying inheritance
      rules, use {{!S.Core.PropsT.own_properties}[own_properties]},
      {{!S.Core.PropsT.get_own_property}[get_own_property]}, and
      {{!S.Core.PropsT.get_own_property_exn}[get_own_property_exn]}. *)

  (** {2 API} *)

  module type S = sig
    include S  (** @open *)
  end

  type t = (module S)

  (** Create a new loader.

      [root] gives the base directory for the loader.

      [image_files] tells how to handle references to image files during
      loading. [`Check] means fail if the file does not exist; [`Ignore] means
      do nothing. The default is [`Check].

      [property_files] tells how to handle custom properties that reference a
      file. [`Load] means load the file; [`Check] means do not load but fail if
      the file does not exist; [`Ignore] means do nothing. The default is
      [`Load]. *)
  val make :
    ?image_files:[`Check | `Ignore] ->
    ?property_files:[`Load | `Check | `Ignore] ->
    root:string ->
    unit ->
    t
end
