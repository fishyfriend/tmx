type xml

val with_xml_from_channel : in_channel -> (xml -> 'a) -> 'a
val tileset_of_toplevel_xml : xml -> Types.tileset
val template_of_toplevel_xml : xml -> Types.template
val map_of_toplevel_xml : xml -> Types.map
