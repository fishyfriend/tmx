module Encoding = struct
  type t = [`Base64 | `Csv] [@@deriving eq, ord, show]
end

type encoding = Encoding.t

module Compression = struct
  type t = [`Gzip | `Zlib | `Zstd] [@@deriving eq, ord, show]
end

type compression = Compression.t

type t =
  { encoding : Encoding.t option;
    compression : Compression.t option;
    data : Bigstring.t [@main] }
[@@deriving eq, ord, show, make]

let encoding t = t.encoding
let compression t = t.compression
let data t = t.data
