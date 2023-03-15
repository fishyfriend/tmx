type xml

val with_xml_from_channel : in_channel -> (xml -> 'a) -> 'a
val property_of_xml : xml -> Property0.t
val properties_of_xml : xml -> Property0.t list
val object_of_xml : xml -> Object0.t
val data_of_xml : xml -> Data.t
val data_of_xml_chunked : dims:int * int -> xml -> Data.t
val image_of_xml : xml -> Image.t
val tile_of_xml : xml -> Tile0.t
val tileset_of_xml : xml -> Tileset0.t
val template_of_xml : xml -> Template0.t
val layer_of_xml :
  type_:[`Tilelayer | `Objectgroup | `Imagelayer | `Group] -> xml -> Layer0.t
val map_of_xml : xml -> Map0.t
