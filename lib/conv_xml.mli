type xml

val with_xml_from_channel : in_channel -> (xml -> 'a) -> 'a
val property_of_xml : xml -> Core_generic.Property.t
val properties_of_xml : xml -> Core_generic.Property.t list
val object_of_xml : xml -> Core_generic.Object.t
val data_of_xml : xml -> Data.t
val data_of_xml_chunked : dims:int * int -> xml -> Data.t
val image_of_xml : xml -> Image.t
val tile_of_xml : xml -> Core_generic.Tile.t
val tileset_of_xml : xml -> Core_generic.Tileset.t
val template_of_xml : xml -> Core_generic.Template.t
val layer_of_xml :
  type_:[`Tilelayer | `Objectgroup | `Imagelayer | `Group] ->
  xml ->
  Core_generic.Layer.t
val map_of_xml : xml -> Core_generic.Map.t
