#' Get final design
#'
#' Takes \code{default_design} as input and modifies it accordingly.
#' 
#' @param engine Data engine. See function \code{\link{create_schema}} for details.
#' @param default_design See function \code{\link{get_default_design}} for details.
#' @param dimension Rule to include/exclude dimensions. See function \code{\link{create_schema}} for details.
#' @param aggregator Rule to include/exclude aggregators for columns. See function \code{\link{create_schema}} for details.
#' @param debug Print additional information useful for debugging.
#' @examples
#' get_final_design('R',get_default_design('R',
#'  get_table_design('R','german_credit'),primary_key='id'),aggregator="'000000'", debug=TRUE)
#' get_final_design('R',get_default_design('R',
#'  get_table_design('R','german_credit'),primary_key='id'),dimension="0")
#' get_final_design('R',
#'  get_default_design('R',get_table_design('R','big_portfolio'),primary_key='id'))
#' @export 

get_final_design <- function(engine, default_design, dimension=NA, aggregator=NA, debug=FALSE) {
  
  if(debug) cat('Creating final design. \n')
  
  final_design <- default_design
  
  if(!(is.na(dimension))) {
    sql <- paste("select name, schema, class, type, ",dimension," as dimension, aggregator, mondrian_type, is_primary_key, clean_name from final_design")
    final_design <- sqldf(sql, drv='SQLite')
  }    

  if(!(is.na(aggregator))) {
    sql <- paste("select name, schema, class, type, dimension, ",aggregator," as aggregator, mondrian_type, is_primary_key, clean_name from final_design")
    final_design <- sqldf(sql, drv='SQLite')
  }      

  if(!all(nchar(final_design$aggregator) == 6)) {
    stop('Cannot create final design. Some aggregator is not defined as string with 6 digits. Check default mapping and aggregator argument.')
  }
  
  if(debug) {
    cat('   Final design created. Number of rows:', nrow(final_design),'\n')
    cat('     Requested number of dimensions: ', sum(final_design$dimension),'\n')
    cat('     Requested number of measures: ', sum(as.integer(unlist(strsplit(final_design$aggregator,split='')))),'\n')
    cat('     Columns with at least one measure: ' , sum(as.integer(final_design$aggregator) > 0),'\n')
    cat('     Columns without dimension and measure: ', sum(!final_design$dimension & as.integer(final_design$aggregator) == 0 ),'\n')
    if (sum(!final_design$dimension & as.integer(final_design$aggregator) == 0) > 0) {
      cat('       |- List: ', final_design$name[!final_design$dimension & as.integer(final_design$aggregator) == 0 ],'\n')
    }
  }
  final_design
     
}
