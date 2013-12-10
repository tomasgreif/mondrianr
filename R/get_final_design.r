#' Get final design
#'
#' Takes \code{default_design} as input and modifies it accordingly.
#' 
#' @param engine Data engine. See function \code{\link{create_schema}} for details.
#' @param default_design See function \code{\link{get_default_design}} for details.
#' @param dimension Rule to include/exclude dimensions. See function \code{\link{create_schema}} for details.
#' @param aggregator Rule to include/exclude aggregators for columns. See function \code{\link{create_schema}} for details.
#' @param debug Print additional information useful for debugging.
#' 
#' @export 

get_final_design <- function(engine, default_design, dimension=NA, aggregator=NA, debug=FALSE) {
  
  final_design <- default_design
  
  if(!(is.na(dimension))) {
    sql <- paste("select name, schema, class, type, ",dimension," as dimension, aggregator, clean_name from final_design")
    final_design <- sqldf(sql, drv='SQLite')
  }    

  if(!(is.na(aggregator))) {
    sql <- paste("select name, schema, class, type, dimension, ",aggregator," as aggregator, clean_name from final_design")
    final_design <- sqldf(sql, drv='SQLite')
  }      

  if(debug) cat('Final design created. \n')
  
  final_design
     
}

#

#default_designx <-  get_default_design('PostgreSQL',get_table_design('PostgreSQL',con=c('tgr','Tms83Grf','tgr','localhost','5432'),table='public.big_portfolio'),primary_key='id')
#load_all()
#get_final_design(engine='PostgreSQL',default_design=default_designx)
#get_final_design(engine='PostgreSQL',default_design=get_default_design('PostgreSQL',get_table_design('PostgreSQL',con=c('tgr','Tms83Grf','tgr','localhost','5432'),table='public.big_portfolio'),primary_key='id'))