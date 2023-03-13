module Encoding : sig
  type t = [`Base64 | `Csv] [@@deriving eq, ord, show]
end

type encoding = Encoding.t

module Compression : sig
  type t = [`Gzip | `Zlib | `Zstd] [@@deriving eq, ord, show]
end

type compression = Compression.t

type t [@@deriving eq, ord, show]

val make : ?encoding:encoding -> ?compression:compression -> bytes -> t
val create : ?encoding:encoding -> ?compression:compression -> int -> t
val encoding : t -> encoding option
val compression : t -> compression option
val bytes : t -> bytes
val of_string : ?encoding:encoding -> ?compression:compression -> string -> t
val of_int32_list : int32 list -> t
