#' Create time dimension for PostgreSQL
#'
#' This function creates time dimension in PostgreSQL.
#' for given data engine.
#' 
#' 
#' @param engine Data engine. See function \code{\link{create_schema}} for details.
#' @param final_design See function \code{\link{get_final_design}} for details.
#' @param table Table with original data
#' @param time_table Name of time dimension (fully qualified table name)
#' @param con See function \code{\link{create_schema}} for details.
#' @param debug See function \code{\link{create_schema}} for details.
#' @param force If \code{TRUE} then existing table will be removed. Use with care! Default is \code{FALSE}
#' @examples \dontrun{
#' create_time_dimension_postgresql('PostgreSQL',final_design,'some_table','dw.timetest',
#'      con=c('user','password','database','host','port'),force=TRUE)
#' }
#' @export 

create_time_dimension_postgresql <- function(engine, final_design, table, time_table,
                                             con=NA, debug=FALSE, force=FALSE) {    
     
  dates <- (paste("select ",
                 "max(greatest(",paste(final_design[final_design$class=='date',1],collapse=','),")) as max_date,", 
                 "min(least(",paste(final_design[final_design$class=='date',1],collapse=','),")) as min_date", 
                 "from ", table))
     
  conn <- dbConnect(drv='PostgreSQL',user=con[1],password=con[2],dbname=con[3],host=con[4],port=con[5])
     
  dates <- dbGetQuery(conn,dates)

  sql <- paste0("
     create table ",time_table," as (
       select 
        gs::date time_date,
        extract(month from gs)::int as month_number,
        extract(quarter from gs)::int as quarter_number,
        extract(year from gs)::int as year_number 
       from 
        generate_series('", dates$min_date ,"'::date,'", dates$max_date,"'::date,'1 day') gs
     );
")  

  if(dbExistsTable(conn,time_table)) {
    if(!force) {
      cat('Table ', time_table, ' already exists and will be used as time dimension. Use force=TRUE to overwrite table.\n')
    } else if (force) {
      cat('Table exists and force is TRUE. Deleting table. \n')
      dbRemoveTable(conn,time_table)
    }
  }

  dbSendQuery(conn,sql)  

  dbDisconnect(conn)
    
  print('end')
}
