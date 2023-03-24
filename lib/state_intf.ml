module type S = sig
  val tilesets : (int * string * Tileset0.t) Seq.t

  (* val add_tileset : string -> Tileset0.t -> (unit, Error.t) result *)
  val add_tileset_exn : string -> Tileset0.t -> unit
  val get_tileset : string -> (int * Tileset0.t) option
  val get_tileset_exn : string -> int * Tileset0.t

  val templates : (string * Template0.t) Seq.t

  (* val add_template : string -> Template0.t -> (unit, Error.t) result *)
  val add_template_exn : string -> Template0.t -> unit
  val get_template : string -> Template0.t option
  val get_template_exn : string -> Template0.t

  val customtypes : Customtype0.t Seq.t

  (* val add_customtype : Customtype0.t -> (unit, Error.t) result *)
  val add_customtype_exn : Customtype0.t -> unit
  val get_customtype : string -> Customtype0.t option
  val get_customtype_exn : string -> Customtype0.t

  val get_class : string -> useas:Class0.useas -> Class0.t option
  val get_class_exn : string -> useas:Class0.useas -> Class0.t

  val maps : (string * Map0.t) Seq.t
  val files : (string * string) Seq.t

  (* val add_file : string -> string -> (unit, Error.t) result *)
  val add_file_exn : string -> string -> unit
  val get_file : string -> string option
  val get_file_exn : string -> string

  val add_map_exn : string -> Map0.t -> unit
  val get_object_tile : Object0.t -> Gid.t -> Tile0.t option
end

module type Intf = sig
  module type S = S module Make () : S
end
