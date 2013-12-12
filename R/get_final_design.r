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
  
  if(debug) cat('Creating final design. \n')
  
  final_design <- default_design
  
  if(!(is.na(dimension))) {
    sql <- paste("select name, schema, class, type, ",dimension," as dimension, aggregator, clean_name from final_design")
    final_design <- sqldf(sql, drv='SQLite')
  }    

  if(!(is.na(aggregator))) {
    sql <- paste("select name, schema, class, type, dimension, ",aggregator," as aggregator, clean_name from final_design")
    final_design <- sqldf(sql, drv='SQLite')
  }      

  if(!all(nchar(final_design$aggregator) == 6)) {
    stop('Cannot create final design. Some aggregator is not defined as string with 6 digits. Check default mapping and aggregator argument.')
  }
  
  if(debug) cat('   Final design created. Number of rows:', nrow(final_design),'\n')
  
  final_design
     
}
