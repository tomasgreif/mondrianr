#' Get schema header
#'
#' Returns schema header for given data engine and table.
#' 
#' @param engine Data engine. See function \code{\link{create_schema}} for details.
#' @param table Table for which header should be returned.
#' @param debug Print additional information useful for debugging.
#' 
#' @examples
#' get_header('R','iris')
#' get_header('PostgreSQL','schema.table')
#' 
#' @export 

get_header <- function(engine, table, debug=NA) {
  
  table_name <- parse_table_name(engine, table)
  
  schema_header <- c(paste0('<Schema name="R Generated Cube">'),
                     paste0(' <Cube name="',standardize_name(table_name[2]),'" visible="true" cache="true" enabled="true">'))
  
  if (engine=='PostgreSQL') {
      # For PostgreSQL, we have to include shcema
      schema_header <- c(schema_header, paste0(' <Table name="',table_name[2],'" schema="',table_name[1],'"></Table>'))
  } else if (engine=='R') {
      schema_header <- c(schema_header, paste0(' <Table name="',table_name[2],'"></Table>'))
  }
  
  
  schema_header <- paste(schema_header, collapse='\n')
  
  schema_header
  
}
  