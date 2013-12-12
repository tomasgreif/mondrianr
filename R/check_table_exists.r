#' Check table existence
#'
#' Checks if table exists for given data engine and connection. Function returns \code{TRUE} when
#' table exists and \code{FALSE} otherwise.
#' 
#' @param engine See function \code{\link{create_schema}} for details.
#' @param table See function \code{\link{create_schema}} for details.
#' @param con See function \code{\link{create_schema}} for details.
#' @param debug See function \code{\link{create_schema}} for details.

check_table_exists<- function(engine, table, con, debug=FALSE) {

  if(engine=='PostgreSQL') {
    drv <- dbDriver("PostgreSQL")
    con <- dbConnect(drv, user=con[1], password=con[2], dbname=con[3], host=con[4], port=con[5])
    table_exists <- dbExistsTable(con,parse_table_name(engine=engine,table=table,debug=debug))
    dbDisconnect(con)
    
    table_exists                     
  }
  
  if(engine=='R') {
    table_exists <- table %in% names(which(unlist(eapply(.GlobalEnv,is.data.frame))))
  }
               
  table_exists                                     
  
}
