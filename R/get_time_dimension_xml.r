#' Get XML for time dimension
#'
#' Returns XML for time dimensions. Creates XML that will be used in Mondrian schema. 
#' 
#' @param engine Data engine. See function \code{\link{create_schema}} for details.
#' @param final_design Final schema design. See function \code{\link{get_final_design}} for details.
#' @param time_table Table with time dimension. See function \code{\link{create_schema}} for details.
#' @param con Connection. See function \code{\link{create_schema}} for details.
#' @param debug Print additional information useful for debugging.
#' 
#' @export 

get_time_dimension_xml <- function(engine,time_table=NA,final_design,con, debug=FALSE) {
  
  if(debug) cat('Creating XML for time dimension. \n')  
  if(debug & is.na(time_table)) cat('   No time table specified, there is nothing to create.\n')
  
  time_dimension_xml <- character(0)
  
  time_dimension  <- final_design[final_design$class=='date',]
  time_dimensions <- nrow(time_dimension)

  if(time_dimensions > 0 & !is.na(time_table)) {
  
    table_name <- parse_table_name(engine, time_table)

    if(!is.na(table_name[1])) {
      schema <- paste0(' schema="',table_name[1],'"')
    } else {
      schema <- character(0)
    }
  
    time_dimension_xml <- paste0('
     <Dimension type="TimeDimension" visible="true" foreignKey="',time_dimension$name,'" highCardinality="false" name="',time_dimension$clean_name,'">
       <Hierarchy name="Time Hierarchy" visible="true" hasAll="true" primaryKey="time_date">
       <Table name="',table_name[2],'"', schema, '></Table>
        <Level name="Year" visible="true" column="year_number" ordinalColumn="year_number" type="Integer" internalType="int" uniqueMembers="false" levelType="TimeYears" hideMemberIf="Never">
        </Level>
        <Level name="Quarter" visible="true" column="quarter_number" ordinalColumn="quarter_number" type="Integer" uniqueMembers="false" levelType="TimeQuarters" hideMemberIf="Never">
        </Level>
        <Level name="Month" visible="true" column="month_number" ordinalColumn="month_number" type="Integer" uniqueMembers="false" levelType="TimeMonths" hideMemberIf="Never">
        </Level>
       </Hierarchy>
      </Dimension>',collapse='')    
  }
  
  if(debug & !is.na(time_table)) cat('   Number of time dimenions: ',time_dimensions,'\n')  
  if(debug) cat('   Creation of XML for time dimensions finished. \n')  
  
  time_dimension_xml

}

