#' Get default design
#'
#' Combines outputs from default mapping (see \code{\link{get_default_mapping}})) and table layout (see
#' \code{\link{get_table_design}}).
#' 
#' If successful, dataset with the following columns is returned: 
#' \tabular{ll}{
#' Column \tab Description\cr
#' \code{name} \tab Column name \cr
#' \code{class} \tab See \code{\link{get_default_mapping}} \cr
#' \code{type} \tab See \code{\link{get_default_mapping}} \cr
#' \code{dimension} \tab See \code{\link{get_default_mapping}} \cr
#' \code{aggregator} \tab Defines default aggregators for given data type. Character string consisting of \cr
#' \code{clean_name} \tab User-friendly column name. \cr
#' }

#' @param engine Data engine. See function \code{\link{create_schema}} for details.
#' @param table_design Table design as returned by \code{\link{get_table_design}}
#' @param primary_key Primary key. See function \code{\link{create_schema}} for details.
#' @param debug Print additional information useful for debugging.
#' @examples
#' mtcars2 <- mtcars
#' mtcars2$id <- seq(1:nrow(mtcars2))
#' mtcars2$vs <- as.integer(mtcars2$vs)
#' mtcars2$am <- as.integer(mtcars2$am)
#' mtcars2$gear <- as.integer(mtcars2$gear)
#' mtcars2$carb <- as.integer(mtcars2$carb)
#' get_default_design('R',get_table_design('R','mtcars2'),primary_key='id')
#' @export 

get_default_design <- function(engine,table_design,primary_key,debug=FALSE) {
  
  if(debug) cat('Creating default design. \n')  
  
  if(nrow(table_design[table_design$name==primary_key, ])==0) {
    stop('Column defined as primary key does not exist in table design.')
  }
  
  default_mapping <- get_default_mapping(engine)
  
  if(!all(table_design$type %in% default_mapping$type))  {
    warning('The following data types are in table, but not in default mapping:',
            paste(unique(table_design[!(table_design$type %in% default_mapping$type),"type"]),collapse=", "), '\n',
            'You have to extend default mapping defined in function get_default_mapping.'
    )
  }
  
  sql <- paste0(
                "select 
                    td.name,
                    td.schema,
                    dm.class,
                    dm.type,
                    dm.dimension,
                    case when td.name='",primary_key,"' then '010000' else dm.aggregator end as aggregator
                  from 
                    default_mapping dm
                    join table_design td using(type)")
  
  default_design <- sqldf(sql ,drv='SQLite')

  default_design$clean_name <-standardize_name(default_design$name)

  if(debug) cat('   Default design created. Number of rows ', nrow(default_design) ,'\n')
  
  default_design
  
}