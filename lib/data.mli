module Encoding : sig
  type t = [`Base64 | `Csv]

  include Sigs0.StdT with type t := t
end

type encoding = Encoding.t

module Compression : sig
  type t = [`Gzip | `Zlib | `Zstd]

  include Sigs0.StdT with type t := t
end

type compression = Compression.t

include Sigs0.StdT

val make : ?encoding:encoding -> ?compression:compression -> bytes -> t
val create : ?encoding:encoding -> ?compression:compression -> int -> t
val encoding : t -> encoding option
val compression : t -> compression option
val bytes : t -> bytes
val of_string : ?encoding:encoding -> ?compression:compression -> string -> t
val of_int32_list : int32 list -> t
