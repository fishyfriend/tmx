# tmx

`tmx` is an OCaml library for reading data files from the 2D game map editor
[Tiled][tiled].

The library aims for broad coverage of Tiled's [TMX file formats][tmx]. It
provides an imperative context for loading TMX data files and a collection of
immutable types correponding to TMX data structures.

`tmx` emulates the semantics of TMX data structures as they exist in the Tiled
desktop application, including proper application of [custom
property][properties] inheritance and [object templates][templates]. This allows
the attributes of game resources in OCaml to match exactly what is observed in
the editor.

The XML-based map (.tmx), tileset (.tsx), and template (.tx) formats are
currently supported along with the undocumented Custom Types JSON
format.

## Installation

`tmx` will hopefully be published to the main `opam` repository in the future.
Till then, pin to the current version with:

    opam pin tmx https://github.com/fishyfriend/tmx.git#v0.1.0

## Documentation

A convenient reference is available by running `dune build @doc`. The generated
HTML landing page is `_build/default/_doc/_html/index.html`.

The most important documentation is for the `Loader` module. For the TMX core
types, one can generally consult the official [TMX format documentation][tmx];
the library API corresponds closely with it, and most differences are
self-explanatory.

## Features

Currently supported:

  - Read XML maps, tilesets, templates
  - Read custom property definitions from JSON Custom Types files
  - Automatically load dependencies
  - Read custom class and enum properties
  - Apply Tiled domain logic for property inheritance
  - Read both fixed-size and infinite maps
  - Apply object templates
  - Check for missing image files
  - Compute the position and dimensions of tile subimages
  - Access embedded image data
  - See nice parse errors (in case you hand-write your XML)

Possibly supported in the future:

  - Better accessors and iteration functions
  - Read tilemap data compressed with Zstd
  - Load JSON maps (.tmj), tilesets (.tsj), and templates (.tj)
  - Modify and write TMX data

Neither supported nor planned:

  - Embedded tilesets
  - Tiled versions before 1.10
  - Deprecated parts of the TMX format
  - Wang tiles, transformations, etc.
  - Object Types files (use newer Custom Types JSON format instead)

## Example

Check out the source and run `utop` from the top-level directory. Then you can
paste in this example and run it.

```ocaml
#require "tmx";;

open Tmx ;;

module L = (val Loader.make ~root:"test/data" ()) ;;

open L.Core ;;

(* Load a map and its dependencies *)

let m = L.load_map_xml_exn "fixed1.tmx" ;;

(* Inspect the autoloaded tilesets *)

let () =
  List.iter (fun (name, ts) ->
      Printf.printf "Tileset from %s has %d tiles\n"
        name (Tileset.tilecount ts))
    (L.tilesets ())
;;

(* Get tile subimage information *)

let () =
  let ts_name = "single1.tsx" in
  let tile_id = 5 in
  let ts = L.get_tileset_exn ts_name in
  let tile = Tileset.get_tile_exn ts tile_id in
  let png =
    Tile.image tile |> Option.get |> Image.source
    |> function `File x -> x | _ -> assert false in
  let x, y = Tile.(x tile, y tile) in
  let w = Option.get (Tile.width tile) in
  let h = Option.get (Tile.height tile) in
  Printf.printf "Example tile subimage info: ";
  Printf.printf "tileset=%s tile=%d file=%s pos=(%d,%d) dims=%dx%d\n"
    ts_name tile_id png x y w h
;;

(* Inspect a tile layer *)

let tl =
  let l = Map.get_layer_exn m 1 in
  match Layer.variant l with `Tilelayer tl -> tl | _ -> assert false
;;

let () =
  for col = 0 to Tilelayer.width tl - 1 do
    for row = 0 to Tilelayer.height tl - 1 do
      match Tilelayer.tile_at tl ~col ~row with
      | Some (tile, _flags) when Tile.get_property "baz" tile <> None ->
        Printf.printf "baz found at col %d, row %d\n" col row
      | _ -> ()
    done
  done
;;

(* Get autoloaded file contents *)

let () =
  let col, row = 12, 15 in
  let tile, _flags = Option.get (Tilelayer.tile_at tl ~col ~row) in
  match Property.value (Tile.get_property_exn "baz" tile) with
  | `File fname ->
    let msg = String.trim (L.get_file_exn fname) in
    Printf.printf "Tile at col %d row %d file contents: %s\n" col row msg
  | _ -> assert false
;;

(* Read custom class values *)

let () =
  let o_id = 31 in
  let o = Map.get_object_exn m o_id in
  assert (Object.class_ o = Some "myclass");
  assert (Object.get_property "favcol" o = None);
  (* Oops, forgot to load the custom types *)
  let _ = L.import_customtypes_json_exn "propertytypes1.json" in
  (* Now the object inherits properties from the class we just loaded *)
  let favcol =
    match Object.get_property_exn "favcol" o |> Property.value with
    | `Color c -> Color.to_string_rgb c
    | _ -> assert false in
  Printf.printf "Object %d's favorite color is %s\n" o_id favcol
;;

(* Read custom enum values *)

let () =
  let o = Map.get_object_exn m 22 in
  let p = Object.get_property_exn "baz" o in
  assert (Property.propertytype p = Some "myenum");
  let e = L.get_enum_exn "myenum" in
  let v = Property.value p in
  List.iter (fun (label, status) ->
      Printf.printf "flag %s -> %s\n" label (if status then "yes" else "no"))
    (Enum.read_as_alist_exn e v)
;;
```

## Copyright

Copyright 2023 Jacob First.

Licensed under the GNU General Public License version 3. See `LICENSE` file for
details.

*tmx is not part of Tiled.*

[tiled]: http://mapeditor.org
[tmx]: https://doc.mapeditor.org/en/stable/reference/tmx-map-format/#
[tsdl]: https://erratique.ch/software/tsdl
[templates]: https://doc.mapeditor.org/en/stable/reference/tmx-map-format/#template-files
[properties]: https://doc.mapeditor.org/en/stable/manual/custom-properties/
