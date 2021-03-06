#' Get default mapping
#'
#' This function returns data frame with default mapping of data types to schema properties
#' for given data engine.
#' 
#' If successful, dataset with the following columns is returned: 
#' \tabular{ll}{
#' Column \tab Description\cr
#' \code{class} \tab Class of data type. Raw data types are aggregated into high-level groups. For example in PostgreSQL, 
#' data types \code{smallint, integer, bigint} have class \code{integer}. This grouping can be used when 
#' using argument \code{aggregator} in function \code{\link{create_schema}}.\cr 
#' \code{type} \tab Data type as defined by data engine.\cr
#' \code{dimension} \tab If \code{TRUE} than columns with given data type will be used to create dimensions.\cr
#' \code{aggregator} \tab Defines default aggregators for given data type. Character string consisting of 
#' exactly six digits. If \code{1} than aggregator is enabled. If \code{0} then aggregator is disabled.
#' Aggregators are defined in the following order: \code{average, count, distinct-count, maximum, minimum, sum}.\cr
#' }
#' Examples for column \code{aggregator}:
#' \itemize{
#'  \item \code{111111} - All aggregators enabled
#'  \item \code{100001} - Only average and sumenabled
#'  \item \code{010000} - Only count enabled
#'  \item \code{000000} - All aggregators disabled - for columns with given data type there will be no measure.
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

class, type, dimension, aggregator,mondrian_type
'integer','integer',TRUE,'111111','Integer'
'integer','smallint',TRUE,'111111','Integer'
'integer','bigint',TRUE,'111111','Integer'
'numeric','numeric',FALSE,'110111','Numeric'
'numeric','decimal',FALSE,'110111','Numeric'
'numeric','real',FALSE,'110111','Numeric'
'numeric','decimal',FALSE,'110111','Numeric'
'numeric','double precision',FALSE,'110111','Numeric'
'text','character',TRUE,'010000','String'
'text','character varying',TRUE,'010000','String'
'text','text',TRUE,'010000','String'
'date','date',TRUE,'010000','Date'
'bool','boolean',TRUE,'010000','Boolean'
'timestamp','timestamp without time zone',FALSE,'000000','Timestamp'

")

} else if (engine=='R') {
default_mapping <- read.table(header=T, sep=",",colClasses=c('character','character','logical','character'), text="
class, type, dimension, aggregator,mondrian_type
'integer','integer',FALSE,'100111','Integer'
'numeric','numeric',FALSE,'100111','Numeric'
'numeric','double',FALSE,'100111','Numeric'
'text','character',TRUE,'010000','String'
'date','date',TRUE,'010000','Date'
'factor','factor',TRUE,'000000','String'
")  
}

default_mapping

}