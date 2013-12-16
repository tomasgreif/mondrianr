#' Get measure
#'
#' Takes \code{final_design} as input and returns data frame with one row for every enabled aggregator.
#' 
#' If successful, dataset with the following columns is returned: 
#' \tabular{ll}{
#' Column \tab Description\cr
#' \code{name} \tab Class of data type.\cr
#' \code{schema} \tab Database schema. If not specified for given data engine than NA.\cr
#' \code{aggregator_name} \tab Aggregator name as defined in Mondrian schema specification.\cr
#' \code{measure_name} \tab User-friendly name.\cr
#' }
#' 
#' @param final_design Final schema design. See function \code{\link{get_final_design}} for details.
#' @param debug Print additional information useful for debugging.
#' @examples 
#' get_measure(get_final_design('R',
#'  get_default_design('R',get_table_design('R','german_credit'),primary_key='id')))
#' 
#' @export 
get_measure <- function(final_design, debug=FALSE) {

  if(debug) cat('Transforming aggregators into individual measures (get_measure). \n')  
  
  aggregators <- c('avg','count','distinct-count','max','min','sum')
  aggregators_names <- c('Avg','#','Dcnt','Max','Min','Sum')

  measure <- final_design[as.integer(final_design$aggregator)>0 & final_design$is_primary_key==0, ]

  measure <- cbind(do.call("rbind", replicate(6, measure, simplify = FALSE)),id_measure=rep(1:6, each=nrow(measure)))

  measure <- measure[substr(measure$aggregator,measure$id_measure,measure$id_measure)=='1', ]

  measure$aggregator_name <- aggregators[measure$id_measure]
  
  measure$aggregator_clean_name <- aggregators_names[measure$id_measure]
  
  measure$measure_name <- paste0(measure$clean_name,'-',measure$aggregator_clean_name)
  
  primary_key_measure <- sqldf("select name, schema, 'count' as aggregator_name, 'Count (PK)' as measure_name from final_design where is_primary_key =1",
                               drv='SQLite')
  
  measure <- sqldf("select name, schema, aggregator_name, measure_name from measure order by measure_name;", drv='SQLite')
  
  measure <- rbind(primary_key_measure, measure)

  if(debug) cat('   Done. Number of measures:', nrow(measure), '\n')  
  
  measure
  
}
