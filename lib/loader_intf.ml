module type S = sig
  (** {1 Core types} *)

  include Core.S

  (** {1 Loading}

      Each of these functions loads a resource from the file at the specified
      path or returns a previously-loaded resource associated with that path.
      Loaded resources are added to the loader state.

      Paths arguments must be relative; they are interpreted relative to the
      loader's root directory.

      Dependencies are loaded automatically, e.g., [load_map_xmp "a.tmx"] causes
      any tilesets and templates referenced by [a.tmx] to be loaded as well.
      (Files referenced by custom properties are loaded optionally; see
      {!Loader.make} for details.) *)

  val load_tileset_xml : string -> (Tileset.t, Error.t) result
  val load_template_xml : string -> (Template.t, Error.t) result
  val load_map_xml : string -> (Map.t, Error.t) result
  val load_file : string -> (string, Error.t) result
  val load_customtypes_json : string -> (Customtype.t list, Error.t) result

  val load_file_exn : string -> string
  val load_map_xml_exn : string -> Map.t
  val load_tileset_xml_exn : string -> Tileset.t
  val load_template_xml_exn : string -> Template.t
  val load_customtypes_json_exn : string -> Customtype.t list

  (** {1 Unloading}

      These functions remove resources from the loader.

      {b Use with caution.} At present, dependencies are not checked, so
      e.g. unloading a tileset that is used by a currently-loaded map will
      result in broken tile lookups.  *)

  val unload_tileset : string -> unit
  val unload_template : string -> unit
  val unload_customtypes : string -> unit
  val unload_class : string -> useas:Class.useas -> unit
  val unload_file : string -> unit
  val unload_map : string -> unit

  (** {1 Querying loaded resources} *)

  val tilesets : unit -> (string * Tileset.t) list
  val templates : unit -> (string * Template.t) list
  val files : unit -> (string * string) list
  val customtypes : unit -> Customtype.t list
  val maps : unit -> (string * Map.t) list

  val get_tileset : string -> Tileset.t option
  val get_template : string -> Template.t option
  val get_customtypes : string -> Customtype.t list
  val get_class : string -> useas:Class.useas -> Class.t option
  val get_map : string -> Map.t option
  val get_file : string -> string option
  val get_tile : Gid.t -> Tile.t option

  val get_tileset_exn : string -> Tileset.t
  val get_template_exn : string -> Template.t
  val get_class_exn : string -> useas:Class.useas -> Class.t
  val get_map_exn : string -> Map.t
  val get_file_exn : string -> string
  val get_tile_exn : Gid.t -> Tile.t
end

type t = (module S)

module type Intf = sig
  (** The entrypoint for loading and accessing Tiled data.

      A loader is a stateful context that keeps track of loaded resources and
      facilitates lookups among them. Typical usage looks like:

      {[let loader = Loader.make ~root:"test/data" () in
        let module L = (val loader) in
        let m = L.load_map_xml_exn "fixed1.tmx" in
        (* ... *)
      ]}

      The loader applies two important transformations to loaded
      resources. First, it rewrites all paths to be relative to its root
      directory. So, for example, a reference to [../sprite.png] that occurs in
      [a/b/tileset.tsx] is rewritten to [a/sprite.png].

      Second, within a map or template, the loader rewrites all tile GIDs to be
      relative to all known tilesets, rather than just those used by the
      containing map or template. This means that tile GIDs from different maps
      can be treated interchangeably even if those maps use different tilesets.
  *)

  module type S = sig
    (* TODO: This should make doc for [S] appear inline, but doesn't *)
    include S  (** @inline *)
  end

  type t = (module S)

  (** Create a new loader.

      [root] gives the base directory. [load_file_props] tells whether the
      loader should auto-load files for custom properties that have file
      values. *)
  val make : ?load_file_props:bool -> root:string -> unit -> t
end
