module type S = sig
  (** {1 Core types} *)

  include Core.S

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

      String arguments and alist keys refer to the name of the source file, {b
      not} the value of the resource's [name] field (if one exists). *)

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

  (** {1 Unloading resources}

      These functions remove resources from the loader, allowing memory to be
      freed.

      {b Use with caution.} At present, dependencies are not checked, so
      e.g. unloading a tileset that is used by a currently-loaded map may
      result in broken tile lookups.  *)

  val unload_tileset : string -> unit
  val unload_template : string -> unit
  val unload_file : string -> unit
  val unload_map : string -> unit

  (** {1:customtypes Custom types}

      Custom types work differently from other loadable resources. They are
      never auto-loaded and, once loaded, they are accessed by name (as returned
      by {!Customtype.name}) rather than by the name of the source file.

      Multiple custom types with the same name are permitted so long as their
      {!val:Class.useas} fields don't conflict. For this purpose, enums are
      treated as classes with [useas] field equal to [[`Property]]. Thus, for
      example, the following custom types may coexist...

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
  (** The entrypoint for loading and accessing Tiled data.

      Typical usage looks like:

      {[let loader = Loader.make ~root:"path/to/my/game" () in
        let module L = (val loader) in
        let m = L.load_map_xml_exn "level1.tmx" in
        (* ... *)
      ]}

      A loader provides both a stateful context for loading Tiled resources and
      a collection of immutable OCaml types representing Tiled data types. Each
      type is paired with a submodule ([Map], [Object], etc.) providing
      accessors and other functions.

      {2 Transformations}

      When loading resources, the loader applies two transformations worth
      noting. First, it rewrites all paths to be relative to its root
      directory. So, for example, a reference to [../sprite.png] that occurs in
      [a/b/tileset.tsx] is rewritten to [a/sprite.png]. This avoids the need to
      traverse nested data structures to infer the full path.

      Second, within a map or template, the loader rewrites all tile GIDs to be
      relative to all known tilesets rather than just those used by the
      containing map or template. This allows tile GIDs from different maps to
      be treated interchangeably even if those maps use different tilesets.

      {2 Semantics}

      The functions on Tiled data types simulate the semantics of the equivalent
      data types in the Tiled desktop application, notably by providing tile GID
      and custom property lookups based on the current loader context.

      This convenience comes with the caveat that accessor functions may return
      different values after the loader state changes. For example, if loading a
      new Custom Types file would result in new properties being inherited by
      some map object, code like the following might succeed:

      {[(* ... continuing the previous snippet *)
        let o = L.Map.get_object_exn m 42 in
        assert (L.Object.get_property "active" o = None);
        let _ = L.import_customtypes_json_exn "propertytypes.json" in
        assert (L.Object.get_property "active" o =
                L.Property.make ~name:"active" ~value:(`Bool true) ())
      ]}

      In this case, the solution to avoid surprises is to ensure that all
      required Custom Types are loaded prior to using property values. *)

  (** {2 API} *)

  module type S = sig
    (* TODO: This should make doc for [S] appear inline, but doesn't *)
    include S  (** @inline *)
  end

  type t = (module S)

  (** Create a new loader.

      [root] gives the base directory for the loader.

      [image_files] tells how to handle references to image files during
      loading. [`Check] means fail if the file does not exist; [`Ignore] means
      do nothing. The default is [`Check].

      [property_files] tells how to handle custom properties that reference a
      file. [`Load] means load the file; [`Check] means do not load, but fail if
      the file does not exist; [`Ignore] means do nothing. The default is
      [`Load]. *)
  val make :
    ?image_files:[`Check | `Ignore] ->
    ?property_files:[`Load | `Check | `Ignore] ->
    root:string ->
    unit ->
    t
end
