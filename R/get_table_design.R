#' Get table design
#'
#' This function returns data frame with structure of specified table
#' for given data engine.
#' If successful, dataset with the following columns is returned: 
#' \tabular{ll}{
#' Column \tab Description\cr
#' \code{name} \tab Class of data type.\cr
#' \code{type} \tab Data type as defined by data engine. This is used to map data type to default schema
#' properties as defined in \code{\link{get_default_mapping}}.\cr
#' \code{schema} \tab Database schema. If not specified for given data engine than NA
#' }
#' @param engine Data engine. See function \code{\link{create_schema}} for details.
#' @param table Table to be analyzed.
#' @param con Connection. See function \code{\link{create_schema}} for details.
#' @param debug Print additional information useful for debugging.
#' @examples \dontrun{
#' get_table_design(engine='R', table='iris')
#' get_table_design(engine='PostgreSQL',table='public.big_portfolio',con=c('usr','pwd','db','host','port'))
#' }
#' @export 

get_table_design <- function(engine=NA, table=NA, con=NA, debug=FALSE) {
  
  if(debug) cat('Preparing table design for table:',table,'\n')
  
  if(engine=='PostgreSQL') {
    
    table_name<-paste(parse_table_name(engine=engine,table=table),collapse='.')
    
    options(sqldf.RPostgreSQL.user = con[1], 
            sqldf.RPostgreSQL.password = con[2],
            sqldf.RPostgreSQL.dbname = con[3],
            sqldf.RPostgreSQL.host = con[4], 
            sqldf.RPostgreSQL.port = con[5])
    
    # -------- Get data from information schema
    table_design <- sqldf(paste0("select 
                                    column_name as name, 
                                    data_type as type,
                                    table_schema as schema
                                 from 
                                  information_schema.columns 
                                 where 
                                  table_schema || '.' || table_name = '",table_name,"'"),drv='PostgreSQL')  
  }
  
  if(engine=='R') {
    table_design <- data.frame(type=sapply(get(table), typeof), 
                               datetype=sapply(names(get(table)), function(x) inherits(get(table)[[x]], "Date")),
                               is_factor=sapply(get(table), is.factor))
    table_design$name <- row.names(table_design)
    row.names(table_design) <- NULL
    table_design <- sqldf("select name, case when is_factor then 'factor' when datetype then 'date' else type end as type from table_design", drv='SQLite')
    table_design$schema <- NA
  }

  if(debug) cat('   Table design created. Number of columns:', nrow(table_design), '\n')
  
  table_design

}