#' Get XML for measures
#'
#' Returns XML for measures
#' 
#' Takes output from function \code{get_measure} and creates XML that will be used in Mondrian schema.
#' 
#' @param measure Data frame with measures. See \code{\link{get_measure}} for details.
#' @param debug Print additional information useful for debugging.
#' @examples
#' measure <- get_measure(get_final_design('R',get_default_design('R',get_table_design('R','german_credit'),primary_key='id')))
#' get_measure_xml(measure)
#' 
#' @export 

get_measure_xml <- function(measure, debug=FALSE) {
  
  if(debug) cat('Creating XML for measures. \n')
  
  measure_xml <- paste0(
    '<Measure name="',measure$measure_name,'" column="',measure$name,'" aggregator="',measure$aggregator_name,'" visible="true"></Measure>'
    , collapse='\n')
  
  if(debug) cat('   XML for measures created. \n')
  
  measure_xml
  
}