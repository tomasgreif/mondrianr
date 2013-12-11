#' Get XML for measures
#'
#' Returns XML for measures
#' 
#' Takes output from function \code{get_measure} and creates XML that will be used in Mondrian schema.
#' 
#' @param measure Data frame with measures. See \code{\link{get_measure}} for details.
#' @param debug Print additional information useful for debugging.
#' @examples
#' get_data_source_definition('R','mtcars','~/schema.xml')
#' get_data_source_definition('PostgreSQL','big_portfolio.xml','~/schema.xml',c('usr','pwd','db','host','port'))
#' mtcars2 <- mtcars
#' mtcars2$id <- seq(1:nrow(mtcars2))
#' mtcars2$vs <- as.integer(mtcars2$vs)
#' mtcars2$am <- as.integer(mtcars2$am)
#' mtcars2$gear <- as.integer(mtcars2$gear)
#' mtcars2$carb <- as.integer(mtcars2$carb)
#' measure <- get_measure(get_final_design('R',get_default_design('R',get_table_design('R','mtcars2'),primary_key='id')))
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