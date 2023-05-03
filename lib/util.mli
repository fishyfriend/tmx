module Option : sig
  val protect : ('a -> 'b) -> 'a -> 'b option

  module Infix : sig
    val ( >>= ) : 'a option -> ('a -> 'b option) -> 'b option
    val ( >|= ) : 'a option -> ('a -> 'b) -> 'b option
    val ( |? ) : 'a option -> 'a -> 'a
    val ( >>? ) : 'a option -> (unit -> 'a option) -> 'a option
    val ( >|? ) : 'a option -> (unit -> 'a) -> 'a
  end
end

module Error : sig
  val protect : ('a -> 'b) -> 'a -> ('b, Error.t) result
  val throw : Error.t -> _
  val invalid_arg : string -> string -> _
  val nested_template : unit -> _
  val base64 : string -> _
  val json_parse : ?fname:string -> string list -> string -> _
  val xml_parse : ?fname:string -> string list -> string -> _
  val gzip : Ezgzip.error -> _
  val zlib : Ezgzip.Z.error -> _
  val zstd : unit -> _
  val duplicate : string -> string -> _
  val not_found : string -> string -> _
  val file_not_found : string -> _
  val other : exn -> _
end

module Filename : sig
  val canon : string -> string
  val reloc : from_dir:string -> to_dir:string -> string -> string
end

val min_format_version : int * int
