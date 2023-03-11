module Encoding : sig
  type t = [`Base64 | `Csv] [@@deriving eq, ord, show]
end

type encoding = Encoding.t

module Compression : sig
  type t = [`Gzip | `Zlib | `Zstd] [@@deriving eq, ord, show]
end

type compression = Compression.t

type t [@@deriving eq, ord, show]

val make : ?encoding:encoding -> ?compression:compression -> Bigstring.t -> t
val encoding : t -> encoding option
val compression : t -> compression option
val data : t -> Bigstring.t
