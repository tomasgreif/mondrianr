#' Parse table name
#'
#' This function returns parsed table name. Given table name as a string, it always returnes vector
#' with exactly two elements. First element is schema, second element is table name. When schema is not
#' defined then first element is \code{NA}
#' @param engine Data engine. See function \code{\link{create_schema}} for details.
#' @param table Table name to be parsed.
#' @param debug Print additional information useful for debugging.
#' @examples
#' parse_table_name(engine='PostgreSQL',table='public.test')
#' parse_table_name(engine='PostgreSQL',table='my_schema.test')
#' parse_table_name(engine='PostgreSQL',table='test')
#' parse_table_name(engine='R',table='iris')
#' @export 

parse_table_name <- function(engine=NA, table=NA, debug=FALSE) {

  if(is.na(table)) {
    stop("Table is not defined.")
  }
  
  if(engine=='PostgreSQL') {

    if (nchar(gsub("[^.]","", table)) > 1) {
      stop("Table name can contaion only one '.' (dot). Table name for PostgreSQL should be in the form 'schema.table'.")
    }

    if  (nchar(gsub("[^.]","", table)) == 1) {
      # If one dot, we can return schema and table
      table_internal <- (strsplit(table,'.',fixed=T))[[1]][2]
      schema_internal <- (strsplit(table,'.',fixed=T))[[1]][1]
      table_name <- c(schema_internal, table_internal)    
    }

    if  (nchar(gsub("[^.]","", table)) == 0) {
      # No dot, we will asumme public schema
      table_name <- c('public', table)    
    }    
    
  }

  if(engine=='R') {
    table_name <- c(NA, table)
  }
  
  table_name


}
