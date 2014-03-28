#' Creates data source definition for Saiku
#'
#' Returns valid data source definition for Saiku and writes it to destination
#' 
#' @param engine Data engine. See function \code{\link{create_schema}} for details.
#' @param table Table for which connection file should be created (used to generate name).
#' @param schema_dest Where schema definition will be stored. Valid path, including file name with schema.
#' @param data_source_dest Where data source definition file should be written.
#' @param con Connection. See \code{\link{create_schema}} for details.
#' @param security_type Security type for Saiku. See \code{\link{create_schema}} for details.
#' @param debug Print additional information useful for debugging.
#' @examples \dontrun{
#' create_data_source_definition('R','mtcars','~/schema.xml','~/data_source')
#' create_data_source_definition('PostgreSQL','big_portfolio.xml',
#'  '~/schema.xml','~/data_source',c('usr','pwd','db','host','port'))
#' }
#' @export 
create_data_source_definition <- function(engine, table, schema_dest, data_source_dest, con, security_type, debug=FALSE) {
  
  if(debug) cat('Creating data source definition file. \n')    
  
  if(is.na(data_source_dest) & debug) {
    cat('   Argument data_source_dest is empty, no file will be created.\n')
  }

  if(!is.na(data_source_dest)) {
    if(engine=='PostgreSQL') {
      data_source_definition <- paste0(
        'type=OLAP
        name=PgSQL
        driver=mondrian.olap4j.MondrianOlap4jDriver
        location=jdbc:mondrian:Jdbc=jdbc:postgresql://', con[4],':', con[5],'/', con[3],'; \\
        Catalog=', schema_dest,';JdbcDrivers=org.postgresql.Driver;
        username=', con[1],'
        password=', con[2]
      )
       if(!is.na(security_type)) {
         data_source_definition <- paste0(data_source_definition,'\n',
         'security.enabled=true
         security.type=', security_type)
       }
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

    writeLines(data_source_definition, con=data_source_dest)
    
    if(debug) cat('   Data source definition file written to destination.\n')
    
  }
  
  if(debug) cat('   Creation of data source definition file finished. \n')  
  
  TRUE
}