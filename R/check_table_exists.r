#' Check table existence
#'
#' Checks if table exists for given data engine and optional connection. Function returns \code{TRUE} when
#' table exists and \code{FALSE} otherwise. This function does not check that table contains any data
#' or columns at all.
#' 
#' @param engine See function \code{\link{create_schema}} for details.
#' @param table See function \code{\link{create_schema}} for details.
#' @param con Required for PostgreSQL. See function \code{\link{create_schema}} for details.
#' @param debug See function \code{\link{create_schema}} for details.
#' @examples
#' check_table_exists('R','iris')
#' 
#' \dontrun{
#' check_table_exists('PostgreSQL','table',c('usr','pwd','db','host','port') }
#' 
#' @export

check_table_exists<- function(engine, table, con=NA, debug=FALSE) {

  if(engine=='PostgreSQL') {
    drv <- dbDriver("PostgreSQL")
    con <- dbConnect(drv, user=con[1], password=con[2], dbname=con[3], host=con[4], port=con[5])
    table_exists <- dbExistsTable(con,parse_table_name(engine=engine,table=table,debug=debug))
    dbDisconnect(con)
    
    table_exists                     
  }
  
  if(engine=='R') {
    table_exists <- exists(table) && is.data.frame(get(table))
  }
               
  table_exists                                     
  
}
