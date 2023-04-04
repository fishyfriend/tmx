type xml

val with_xml_from_channel : in_channel -> (xml -> 'a) -> 'a
val property_of_xml : xml -> Basic.Property.t
val properties_of_xml : xml -> Basic.Property.t list
val object_of_xml : xml -> Basic.Object.t
val data_of_xml : xml -> Data.t
val data_of_xml_chunked : dims:int * int -> xml -> Data.t
val image_of_xml : xml -> Image.t
val tile_of_xml : xml -> Basic.Tile.t
val tileset_of_xml : xml -> Basic.Tileset.t
val template_of_xml : xml -> Basic.Template.t
val layer_of_xml :
  type_:[`Tilelayer | `Objectgroup | `Imagelayer | `Group] ->
  xml ->
  Basic.Layer.t
val map_of_xml : xml -> Basic.Map.t
