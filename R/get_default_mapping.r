#' Get default mapping
#'
#' This function returns data frame with default mapping of data types to schema properties
#' for given data engine.
#' If successful, dataset with the following columns is returned: 
#' \tabular{ll}{
#' Column \tab Description\cr
#' \code{class} \tab Class of data type.\cr
#' \code{type} \tab Data type as defined by data engine.\cr
#' \code{dimension} \tab If \code{TRUE} than given data type will be used to create dimensions.\cr
#' \code{aggregator} \tab Defines default aggregators for given data type. Character string consisting of 
#' exactly six digits. If \code{1} than aggregator is enabled. If \code{0} then aggregator is disabled.
#' Aggregators are defined in the following order: \code{average, count, distinct-count, maximum, minimum, sum}.\cr
#' }

#' @param engine Data engine. See function \code{\link{create_schema}} for details.
#' @param debug Print additional information useful for debugging.
#' @examples
#' get_default_mapping('PostgreSQL')
#' get_default_mapping('R')
#' get_default_mapping('R', debug=TRUE)
#' @export 

get_default_mapping <- function(engine=NA, debug=FALSE) {

if(engine=='PostgreSQL') {  
# Order of aggregators - average, count, count distinct, max, min, sum
default_mapping <- read.table(header=T, sep=",",colClasses=c('character','character','logical','character'), text="
class, type, dimension, aggregator
'integer','integer',TRUE,'111111'
'integer','smallint',TRUE,'111111'
'integer','bigint',TRUE,'111111'
'numeric','numeric',FALSE,'110111'
'numeric','decimal',FALSE,'110111'
'numeric','real',FALSE,'110111'
'numeric','decimal',FALSE,'110111'
'numeric','double precision',FALSE,'110111'
'text','character',TRUE,'010000'
'text','character varying',TRUE,'010000'
'text','text',TRUE,'010000'
'date','date',TRUE,'010000'
")

} else if (engine=='R') {
default_mapping <- read.table(header=T, sep=",",colClasses=c('character','character','logical','character'), text="
class, type, dimension, aggregator
'integer','integer',TRUE,'111111'
'numeric','numeric',FALSE,'110111'
'numeric','double',FALSE,'110111'
'text','character',TRUE,'010000'
'date','date',TRUE,'010000'
")  
}

default_mapping

}