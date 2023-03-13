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
    bytes : bytes [@main] }
[@@deriving eq, ord, show, make]

let create ?encoding ?compression n =
  make ?encoding ?compression (Bytes.create n)

let encoding t = t.encoding
let compression t = t.compression
let bytes t = t.bytes

let read_base64 s =
  try Base64.decode_exn s with Invalid_argument msg -> Util.base64 msg

let read_gzip s =
  match Ezgzip.decompress s with Ok s -> s | Error (`Gzip e) -> Util.gzip e

let read_zlib s =
  match Ezgzip.Z.decompress ~header:true s with
  | Ok s -> s
  | Error (`Zlib e) -> Util.zlib e

let read_zstd _ = Util.zstd ()

let read_csv s =
  let cells = String.split_on_char ',' s in
  let n_cells = List.length cells in
  let next = List.to_seq cells |> Seq.to_dispenser in
  let bytes = Bytes.create (n_cells * 4) in
  for i = 0 to n_cells - 1 do
    let gid = next () |> Option.get |> String.trim |> Int32.of_string in
    Bytes.set_int32_ne bytes (i * 4) gid
  done ;
  String.of_bytes bytes

let of_string ?encoding ?compression s =
  let s =
    match (encoding, compression) with
    | None, None -> s
    | None, Some `Gzip -> read_gzip s
    | None, Some `Zlib -> read_zlib s
    | None, Some `Zstd -> read_zstd s
    | Some `Base64, None -> read_base64 s
    | Some `Base64, Some `Gzip -> read_base64 s |> read_gzip
    | Some `Base64, Some `Zlib -> read_base64 s |> read_zlib
    | Some `Base64, Some `Zstd -> read_base64 s |> read_zstd
    | Some `Csv, None -> read_csv s
    | Some `Csv, Some c -> Util.invalid_arg "compression" (Compression.show c)
  in
  let bytes = Bytes.unsafe_of_string s in
  make ?encoding ?compression bytes

let of_int32_list xs =
  let n = List.length xs in
  let next = List.to_seq xs |> Seq.to_dispenser in
  let bytes = Bytes.create (n * 4) in
  for i = 0 to n - 1 do
    let gid = next () |> Option.get in
    Bytes.set_int32_ne bytes (i * 4) gid
  done ;
  make bytes
