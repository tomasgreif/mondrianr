#' Creates final Mondrian XML schema
#'
#' Creates final Mondrian XML schema and writes it to specified destination
#' 
#' @param engine Data engine. See function \code{\link{create_schema}} for details.
#' @param table Table for which schema should be created
#' @param time_dimension_xml Final XML for time dimension
#' @param dimension_xml Final XML for dimensions
#' @param measure_xml Final XML for measures
#' @param calculated_member_xml Final XML for calculated members
#' @param schema_dest Schema destination
#' @param debug Print additional information useful for debugging.
#' 
#' @export 
create_schema_xml <- function(engine, table, time_dimension_xml, dimension_xml, measure_xml, calculated_member_xml, schema_dest, debug=FALSE) {
  
  if(debug) cat('Creating final Mondrian XML schema. \n')    
  
  schema_definition <- paste0(get_header(engine, table),'\n', time_dimension_xml, '\n', dimension_xml,'\n\n',measure_xml,'\n',
                              calculated_member_xml,'\n',
                              get_footer())
  writeLines(schema_definition, con=schema_dest)

  if(debug) cat('   Cube written to destination. \n')
  if(debug) cat('   XML Schema definition finished \n')  
  
  TRUE
}