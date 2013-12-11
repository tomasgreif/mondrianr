#' Get XML for generic dimension
#'
#' Returns XML for generic dimensions Creates XML that will be used in Mondrian schema. Generic dimensions
#' are currently all columns where dimension is \code{TRUE} in \code{final_design} and column is not of \code{class}
#' date.
#' 
#' @param engine Data engine. See function \code{\link{create_schema}} for details.
#' @param final_design Final schema design. See function \code{\link{get_final_design}} for details.
#' @param table Table for which connection file should be created (used to generate name).
#' @param time_table Time table. See function \code{\link{create_schema}} for details.
#' @param primary_key Primary key of \code{table}.
#' @param debug Print additional information useful for debugging.
#' @examples
#' mtcars2 <- mtcars
#' mtcars2$id <- seq(1:nrow(mtcars2))
#' mtcars2$vs <- as.integer(mtcars2$vs)
#' mtcars2$am <- as.integer(mtcars2$am)
#' mtcars2$gear <- as.integer(mtcars2$gear)
#' mtcars2$carb <- as.integer(mtcars2$carb)
#' final_design <- get_final_design('R',get_default_design('R',get_table_design('R','mtcars2'),primary_key='id'))
#' get_generic_dimension_xml('R',final_design, 'mtcars2','id') 
#' 
#' @export 

get_generic_dimension_xml <- function(engine, final_design, table, time_table=NA, primary_key, debug=FALSE) {

  if(debug) cat('Creating XML for generic dimensios. \n')
  
  dimension   <- final_design[final_design$dimension, ]
  dimensions  <- nrow(dimension)

  if(dimensions > 0) {
    table_name <- parse_table_name(engine, table)
    
    if (!is.na(time_table)) {
      dimension <- dimension[dimension$class != 'date', ]  
    }
    
    if(!is.na(table_name[1])) {
      schema <- paste0(' schema="',table_name[1],'"')
    } else {
      schema <- character(0)
    }

    dimension_xml <- paste('
    <Dimension type="StandardDimension" visible="true" foreignKey="',primary_key,'" highCardinality="false" name="',dimension$clean_name,'">
     <Hierarchy name="',dimension$clean_name,'" visible="true" hasAll="true" primary_key="',primary_key,'">
     <Table name="',table_name[2],'"', schema ,'></Table>
      <Level name="',dimension$clean_name,'" visible="true" column="',dimension$name,'" type="String" uniqueMembers="false" levelType="Regular" hideMemberIf="Never">
      </Level>
     </Hierarchy>
    </Dimension>',sep='', collapse='')  
    
    if(debug) cat('   Number of generic dimensions: ',dimensions,'\n')    
    if(debug) cat('   XML for generic dimensions created. \n')    

  }

  
  dimension_xml

}


  
