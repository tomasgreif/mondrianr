#' Prepare infrastructure
#'
#' Prepare infrastructure. This includes all steps necessary before we start creating schema. For some data
#' engines there is nothing to prepare
#' 
#' @param engine Data engine. See function \code{\link{create_schema}} for details.
#' @param table Table with data. See function \code{\link{create_schema}} for details.
#' @param time_table Table with time dimension. See function \code{\link{create_schema}} for details.
#' @param debug Print additional information useful for debugging.
#' 
#' @export 

prepare_infrastructure <- function(engine=NA, table=NA, time_table=NA, debug=FALSE) {

  if(debug) cat('Preparing infrastructure. \n')
  
  if(engine=='R') {
    # For R engine, we will write data to temporary SQLite database (because mondrian can connect to SQLite database)
    file.remove('__this_is_temporary_db__.db')
    sqlite    <- dbDriver("SQLite")
    tmpdb <- dbConnect(sqlite,"__this_is_temporary_db__.db")           
    
    dbWriteTable(tmpdb,table,get(table))
    
    #------ Generate time dimension
    if (!is.na(time_table)) {
      time_design <- get_table_design('R',table=table)
      date_columns <- time_design[time_design$type=='date', 'name']
      
      min_date <- min(apply(get(table)[,date_columns], MARGIN=2, min,na.rm=TRUE), na.rm=TRUE)
      max_date <- min(apply(get(table)[,date_columns], MARGIN=2, max,na.rm=TRUE), na.rm=TRUE)
      
      ddiff <- as.integer(as.Date(max_date)) - as.integer(as.Date(min_date))
      tmp_time <- data.frame(time_date = as.Date(min_date) + seq(1:ddiff))
      tmp_time$month_number <- as.integer(format(tmp_time$time_date, "%m"))
      tmp_time$year_number <- as.integer(format(tmp_time$time_date, "%Y"))
      tmp_time$quarter_number[tmp_time$month_number %in% 1:3] <- 1
      tmp_time$quarter_number[tmp_time$month_number %in% 4:6] <- 2
      tmp_time$quarter_number[tmp_time$month_number %in% 7:9] <- 3
      tmp_time$quarter_number[tmp_time$month_number %in% 10:12] <- 4
      
      dbWriteTable(tmpdb,time_table,tmp_time)           
    }
    
    #------ Disconnect     
    dbDisconnect(tmpdb)      
  }

  if(debug) cat('   Infrastructure prepared. \n')
  
  TRUE
}
