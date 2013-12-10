#' Get data source definition for Saiku
#'
#' Returns valid data source definition for Saiku
#' 
#' @param engine Data engine. See function \code{\link{create_schema}} for details.
#' @param table Table for which connection file should be created (used to generate name).
#' @param schema_dest Where schema definition will be stored. Valid path, including file name with schema.
#' @param con Connection. See \code{\link{create_schema}} for details.
#' @param debug Print additional information useful for debugging.
#' @examples \dontrun{
#' get_data_source_definition('R','mtcars','~/schema.xml')
#' get_data_source_definition('PostgreSQL','big_portfolio.xml','~/schema.xml',c('usr','pwd','db','host','port'))
#' }
#' @export 
get_data_source_definition <- function(engine,table,schema_dest,con, debug=FALSE) {
  
  if(engine=='PostgreSQL') {
    data_source_definition <- paste0(
      'type=OLAP
      name=',table,'
      driver=mondrian.olap4j.MondrianOlap4jDriver
      location=jdbc:mondrian:Jdbc=jdbc:postgresql://', con[4],':', con[5],'/', con[3],'; \\
      Catalog=', schema_dest,';JdbcDrivers=org.postgresql.Driver;
      username=', con[1],'
      password=', con[2]
    )    
  }

  if(engine=='R') {
    data_source_definition <- paste0(
      'type=OLAP
      name=RData 
      driver=mondrian.olap4j.MondrianOlap4jDriver
      location=jdbc:mondrian:Jdbc=jdbc:sqlite:',getwd(),'/__this_is_temporary_db__.db; \\
      Catalog=',schema_dest,';JdbcDrivers=org.sqlite.JDBC 
')
  }
  
  if(debug) cat('Data source definition created. \n')  
  
    data_source_definition
}