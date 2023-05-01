module Error : module type of Error
module Color : module type of Color
module Gid : module type of Gid
module Loader : module type of Loader

(** The minimum TMX format version supported. *)
val min_format_version : int * int
