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
#' \code{aggregator} \tab Defines default aggregators for given data type. Character string consisting of exactly 6 digits.\cr
#' \code{is_primary_key} \tab Flag indicating column with primary key. \code{1} if primary key, \code{0} otherwise. \cr
#' \code{clean_name} \tab User-friendly column name. \cr
#' }

#' @param engine Data engine. See function \code{\link{create_schema}} for details.
#' @param table_design Table design as returned by \code{\link{get_table_design}}
#' @param primary_key Primary key. See function \code{\link{create_schema}} for details.
#' @param debug Print additional information useful for debugging.
#' @examples
#' get_default_design('R',get_table_design('R','german_credit'),primary_key='id')
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
                    dm.mondrian_type,
                    case when td.name='",primary_key,"' then '010000' else dm.aggregator end as aggregator,
                    case when td.name='",primary_key,"' then 1 else 0 end as is_primary_key
                  from 
                    default_mapping dm
                    join table_design td using(type)")
  
  default_design <- sqldf(sql ,drv='SQLite')

  default_design$clean_name <-standardize_name(default_design$name)

  if(debug) cat('   Default design created. Number of rows ', nrow(default_design) ,'\n')
  
  default_design
  
}