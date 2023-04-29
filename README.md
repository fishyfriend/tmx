# tmx

tmx is an OCaml library for reading data files from the 2D game map editor
[Tiled][tiled].

The XML file formats for maps (.tmx), tilesets (.tsx), and object templates
(.tx) are supported, along with the JSON Custom Types format. Support for other
Tiled JSON formats is planned.

## Installation

`dune` and `opam` are required. Install the development version by checking out
source code then running `opam update && opam install .` from the source directory.

The library will be published to the main `opam` repository in the future.

## Documentation

The API strives to correspond closely to the official [TMX format
documentation][tmx]. Most differences are self-explanatory. Some additional
documentation is provided in the interface files, particularly for the `Loader`
module.

A convenient HTML (and other formats) reference is available by running `dune
build @doc`. The generated HTML landing page is
`_build/default/_doc/_html/index.html`.

## Features

  - Read maps, tilesets, and object templates from latest Tiled version (1.10)
  - Autoload tilesets and templates referenced by a map, and tilesets referenced
    by a template
  - Optionally autoload files referenced by custom property values
  - Faithfully apply Tiled's rules for property inheritance
  - Read custom class and enum properties
  - Get the position and dimensions of tile subimages
  - Access embedded image data as raw bytes
  - See nice parse errors (in case you hand-write your XML)

## Planned features

  - Load JSON maps (.tmj), tilesets (.tsj), and templates (.tj)
  - Read tilemap data compressed with Zstd
  - Support for future Tiled versions
  - Better accessors and iteration functions
  - "Generic" loader (see `Loader` docs)
  - Modify and write Tiled objects (maybe)

## Not supported

  - Embedded tilesets
  - Tiled versions before 1.10
  - Object Types files (use Custom Types JSON format instead)
  - Automatic image loading

## Example

```ocaml
open Tmx ;;

module L = (val Loader.make ~root:"test/data" ()) ;;

let m = L.load_map_xml_exn "fixed1.tmx" ;;

(* Inspect autoloaded tilesets *)

let () =
  List.iter (fun (name, ts) ->
      Printf.printf "Tileset from %s has %d tiles\n"
        name (L.Tileset.tilecount ts))
    (L.tilesets ())
;;

(* Get tile subimage information *)

let () =
  let ts_name = "single1.tsx" in
  let tile_id = 5 in
  let ts = L.get_tileset_exn ts_name in
  let tile = L.Tileset.get_tile_exn ts tile_id in
  let png =
    L.Tile.image tile |> Option.get |> L.Image.source
    |> function `File x -> x | _ -> assert false in
  let x, y = L.Tile.(x tile, y tile) in
  let w = Option.get (L.Tile.width tile) in
  let h = Option.get (L.Tile.height tile) in
  Printf.printf "Example tile subimage info: ";
  Printf.printf "tileset=%s tile=%d file=%s pos=(%d,%d) dims=%dx%d\n"
    ts_name tile_id png x y w h
;;

(* Inspect a tile layer *)

let tl =
  let l = L.Map.get_layer_exn m 1 in
  match L.Layer.variant l with `Tilelayer tl -> tl | _ -> assert false
;;

let () =
  for col = 0 to L.Tilelayer.width tl - 1 do
    for row = 0 to L.Tilelayer.height tl - 1 do
      match L.Tilelayer.tile_at tl ~col ~row with
      | Some tile when L.Tile.get_property "baz" tile <> None ->
        Printf.printf "baz found at col %d, row %d\n" col row
      | _ -> ()
    done
  done
;;

(* Get autoloaded file contents *)

let () =
  let col, row = 12, 15 in
  let tile = Option.get (L.Tilelayer.tile_at tl ~col ~row) in
  match L.Property.value (L.Tile.get_property_exn "baz" tile) with
  | `File fname ->
    let msg = L.get_file_exn fname in
    Printf.printf "Tile at col %d row %d file contents: %s\n" col row msg
  | _ -> assert false
;;

(* Read properties with custom types *)

let () =
  let o_id = 31 in
  let o = L.Map.get_object_exn m o_id in
  assert (L.Object.class_ o = Some "abc");
  assert (L.Object.get_property "favcol" o = None);
  (* Oops, forgot to load the custom types *)
  let _ = L.import_customtypes_json_exn "abc-propertytypes.json" in
  (* Now the object inherits properties from the class we just loaded *)
  let favcol =
    match L.Object.get_property_exn "favcol" o |> L.Property.value with
    | `Color c -> Color.to_string_rgb c
    | _ -> assert false in
  Printf.printf "Object %d's favorite color is %s\n" o_id favcol
;;
```

## Copyright

Copyright 2023 Jacob First.

Licensed under the GNU General Public License version 3. See `LICENSE` file for
details.

*tmx is not part of Tiled.*

[tiled]: http://mapeditor.org
[tmx]: https://doc.mapeditor.org/en/stable/reference/tmx-map-format/#
