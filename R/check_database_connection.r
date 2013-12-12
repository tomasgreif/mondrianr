#' Check database existence
#'
#' Checks if database exists for given data engine and connection. Function returns \code{TRUE} when
#' database exists and \code{FALSE} otherwise.
#' 
#' @param engine See function \code{\link{create_schema}} for details.
#' @param con See function \code{\link{create_schema}} for details.

check_database_connection <- function(engine, con) {
  
    options(sqldf.RPostgreSQL.user      = con[1], 
            sqldf.RPostgreSQL.password  = con[2],
            sqldf.RPostgreSQL.dbname    = con[3],
            sqldf.RPostgreSQL.host      = con[4], 
            sqldf.RPostgreSQL.port      = con[5])
    
    out <- tryCatch(
{
  sqldf("select TRUE;")
},
error=function(cond) {
  out <- FALSE
}
    )    
    return(out)
  }
    