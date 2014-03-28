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
#' @param security_roles See function \code{\link{create_schema}} for details.
#' @param debug Print additional information useful for debugging.
#' 
#' @export 
create_schema_xml <- function(engine, table, time_dimension_xml, dimension_xml, measure_xml, calculated_member_xml, schema_dest, security_roles, debug=FALSE) {
  
  if(debug) cat('Creating final Mondrian XML schema. \n') 
  
  security_roles_xml <- character(0)
  
  if(length(security_roles) == 1) {
    security_roles_xml <- paste0(
      '<Role name="', security_roles, '"><SchemaGrant access="none">
      <CubeGrant cube="', standardize_name(parse_table_name(engine, table)[2]),'" access="all"></CubeGrant></SchemaGrant></Role>'
    )
  } else if (length(security_roles) == 2) {
    security_roles_xml <- paste0('
      <Role name="', security_roles[2],'"><SchemaGrant access="none"></SchemaGrant></Role>
      <Role name="', security_roles[1], '"><SchemaGrant access="none">
      <CubeGrant cube="', standardize_name(parse_table_name(engine, table)[2]),'" access="all"></CubeGrant></SchemaGrant></Role>'
    )
  }
  
  schema_definition <- paste0(get_header(engine, table),'\n', time_dimension_xml, '\n', dimension_xml,'\n\n',measure_xml,'\n',
                              calculated_member_xml,'\n',
                              '</Cube>','\n',                              
                              security_roles_xml,'\n',
                              get_footer())
  writeLines(schema_definition, con=schema_dest)

  if(debug) cat('   Cube written to destination. \n')
  if(debug) cat('   XML Schema definition finished \n')  
  
  TRUE
}