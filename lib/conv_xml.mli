type xml

val with_xml_from_channel : in_channel -> (xml -> 'a) -> 'a
val property_of_xml : xml -> Types.property
val properties_of_xml : xml -> Types.property list
val object_of_xml : xml -> Types.object_
val data_of_xml : xml -> Types.data
val data_of_xml_chunked : dims:int * int -> xml -> Types.data
val image_of_xml : xml -> Types.image
val tile_of_xml : xml -> Types.tile
val tileset_of_xml : xml -> Types.tileset
val template_of_xml : xml -> Types.template
val layer_of_xml :
  type_:[`Tilelayer | `Objectgroup | `Imagelayer | `Group] ->
  xml ->
  Types.layer
val map_of_xml : xml -> Types.map
