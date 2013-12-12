#' Check inputs
#'
#' Checks inputs to function \code{\link{create_schema}}. Error is reported when wrong configuration is found,
#' otherwise this function returns nothing.
#' 
#' @param engine See function \code{\link{create_schema}} for details.
#' @param table See function \code{\link{create_schema}} for details.
#' @param primary_key See function \code{\link{create_schema}} for details.
#' @param con See function \code{\link{create_schema}} for details.
#' @param dimension See function \code{\link{create_schema}} for details.
#' @param aggregator See function \code{\link{create_schema}} for details.
#' @param schema_dest See function \code{\link{create_schema}} for details.
#' @param data_source_dest See function \code{\link{create_schema}} for details.
#' @param time_table See function \code{\link{create_schema}} for details.
#' @param debug See function \code{\link{create_schema}} for details.
#' @export 

check_inputs <- function(engine=NA, table=NA, primary_key=NA, con=NA, dimension=NA, aggregator=NA,schema_dest=NA,data_source_dest=NA,
                         time_table=NA,debug=FALSE) {
  
  if(debug) cat('Checking input arguments. \n')
  
  # Currently only R and PostgreSQL as data engine are supported
  if(!(engine %in% c('R','PostgreSQL'))) {
    stop('Engine has to be R or PostgreSQL')
  }
  
  # Table is always required
  if(is.na(table)) {
    stop('Table is required.')
  }
  
  # Primary key is required
  if(is.na(primary_key)) {
    stop('Primary key is required.')
  }
  
  # Schema destination is required
  if(is.na(schema_dest)) {
    stop('Schmea destination is required.')
  }
  
  # If engine is PostgreSQL then connection is required
  if(engine=='PostgreSQL' & all(is.na(con))) {
    stop('For PostgreSQL you have to specify connection.')
  }
  
  # If engine is PostgreSQL then con has to have 5 elements exactly
  if(engine=='PostgreSQL' & length(con) != 5) {
    stop('Connection has to be specified as vector with exactly five elements. See documentation for more details.')
  }

  # If engine is PostgreSQL than we should test that connection is valid
  if(engine=='PostgreSQL') {
    if (!check_database_connection('PostgreSQL',con)) {
      stop("Not valid PostgreSQL connection.") 
    }    
  }
  
  # We should check that table exists
  if(!check_table_exists(engine=engine, table=table, con=con, debug=debug)) {
    stop('Table specified as argument "table" does not exists.')
  }
  
  # If engine is PostgreSQL and time table is specified than it should exist
  if(engine=='PostgreSQL' & !is.na(time_table)) {
    if(!check_table_exists(engine=engine, table=time_table, con=con, debug=debug)) {  
      stop('Table specified as argument "time_table" does not exists.')
    }    
  }

  if(debug) cat('   Everything looks OK so far. \n')  

}