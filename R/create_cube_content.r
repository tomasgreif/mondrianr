#' Create OLAP Schema
#' 
#' Create cube content is internal function of \code{olapr} package. Based on inputs, it will create correct cube for chosen database engine.
#' 
#' @param engine Name of database engine. Currently supports: \code{R, PostgreSQL}
#' @param table_design Data frame with column names and data types. Passed from other functions
#' @param primary_key Passed from other functions
#' @param numeric_aggregators Passed from other functions
#' @param date_aggregators Passed from other functions
#' @param table Passed from other functions
#' @param schema Passed from other functions
#' @param time_table Passed from other functions
#' @param time_schema Passed from other functions
#' @param dimension_data_types Passed from other functions
#' @param numeric_data_types Passed from other functions
#' @export

create_schema <- function(engine, table_design, primary_key, numeric_aggregators=NA, date_aggregators=NA, table,
                              schema, time_table, time_schema, dimension_data_types, numeric_data_types) {
     
  dimension_variables <- table_design[table_design$data_type %in% dimension_data_types,1]
  variables_numeric   <- sort(table_design[table_design$data_type %in% numeric_data_types,1])
  dimension_variables_names   <- standardize_name(dimension_variables)
  variables_numeric   <- variables_numeric[!(variables_numeric %in% primary_key)]
  variables_date      <- table_design[table_design$data_type %in% c('date'),1]
  variables_date_names <- standardize_name(variables_date)     

#----------- Define aggregators for different types of variables

  aggregators <- c('sum','count','min','max','avg','distinct-count')
  aggregators_names <- c('Sum','Cnt','Min','Max','Avg','Dcnt')

 # Numeric aggregators
     if (!all(is.na(numeric_aggregators))) {
          numeric_aggregators <- aggregators[aggregators %in% numeric_aggregators]
          numeric_aggregators_names <- aggregators_names[aggregators %in% numeric_aggregators]
     } else {
          numeric_aggregators <- aggregators
          numeric_aggregators_names  <- aggregators_names
     }

  # Date aggregators
     if (!all(is.na(date_aggregators))) {
          date_aggregatrors <- aggregators[aggregators %in% date_aggregators]
          date_aggregators <- aggregators_names[aggregators %in% date_aggregators]
     } else {
          date_aggregators <- 'count'
          date_aggregators_names  <- '#'
     }

#---------- Define Dimensions

  # Initialize empty vectors
     dimensions_date <- character(0)
     dimensions_varcharr <- character(0)

     time_schema <- if(!is.na(time_schema)) paste0(' schema="',time_schema,'"') else ''
     schema <- if(!is.na(schema)) paste0(' schema="',schema,'"') else ''

  # Generate date hierarchies (time dimension)
     if (length(variables_date) > 0) {
          dimension_date <- paste0('
     <Dimension type="TimeDimension" visible="true" foreignKey="',variables_date,'" highCardinality="false" name="',variables_date_names,'">
      <Hierarchy name="Time Hierarchy" visible="true" hasAll="true" primary_key="time_date">
      <Table name="',time_table,'"',time_schema,'></Table>
       <Level name="Year" visible="true" column="year_number" ordinalColumn="year_number" type="Integer" internalType="int" uniqueMembers="false" levelType="TimeYears" hideMemberIf="Never">
       </Level>
       <Level name="Quarter" visible="true" column="quarter_number" ordinalColumn="quarter_number" type="Integer" uniqueMembers="false" levelType="TimeQuarters" hideMemberIf="Never">
       </Level>
       <Level name="Month" visible="true" column="month_number" ordinalColumn="month_number" type="Integer" uniqueMembers="false" levelType="TimeMonths" hideMemberIf="Never">
       </Level>
      </Hierarchy>
     </Dimension>',collapse='')
     }

  # Gnerate other hierarchies
     if(length(dimension_variables) > 0) {
          dimensions_varchar <- paste('
     <Dimension type="StandardDimension" visible="true" foreignKey="',primary_key,'" highCardinality="false" name="',dimension_variables_names,'">
      <Hierarchy name="',dimension_variables_names,'" visible="true" hasAll="true" primary_key="',primary_key,'">
      <Table name="',table,'"',schema,'></Table>
       <Level name="',dimension_variables_names,'" visible="true" column="',dimension_variables,'" type="String" uniqueMembers="false" levelType="Regular" hideMemberIf="Never">
       </Level>
      </Hierarchy>
     </Dimension>',sep='', collapse='')
     }

  # Merge All dimensions togetgher
     dimensions <- paste0(dimensions_date, dimensions_varchar)

#------------- Generate Measures

  # Generate default measure - count of primary column
     measure_primary_key <- paste(' <Measure name="','Count (PK)','" column="',primary_key,'" aggregator="count" visible="true"></Measure>',sep='')

  # Initialize empty vectors
     numeric_measures <- character(0)     
     date_measures <- character(0)

  # Generate measures for numeric variables
     if(length(variables_numeric) > 0) {
          for (i in seq_along(numeric_aggregators)) {
               numeric_measures <- c(numeric_measures,paste0(' <Measure name="',standardize_name(variables_numeric),'-',numeric_aggregators_names[i]
                                                           ,'" column="',variables_numeric,'" aggregator="',numeric_aggregators[i],'" visible="true"></Measure>'))
          }
     }

  # Generate measures for date variables
     if(length(variables_date) > 0) {
          for (i in seq_along(date_aggregators)) {
               date_measures <- c(date_measures,paste0(' <Measure name="',standardize_name(variables_date),' ',date_aggregators_names[i]
                                                     ,'" column="',variables_date,'" aggregator="',date_aggregators[i],'" visible="true"></Measure>'))
          }
     }

  # Paste measures together in desired order
     measures <- paste0( measure_primary_key,'\n', paste0(sort(c(numeric_measures, date_measures)),collapse='\n'),collapse='\n')

# ---------- Define Header and Footer for cube

     cube_header <- paste0(c(
          paste0('<Schema name="R Generated Cube">'),
          paste0(' <Cube name="',table,'" visible="true" cache="true" enabled="true">'),
          paste0(' <Table name="',table,'"',schema,'></Table>'))
                          ,collapse='\n')

     cube_footer <- '</Cube></Schema>' 

# ---------- Get and return final cube
     
     cube_definition <- paste0(cube_header,'\n',dimensions,'\n\n',measures,'\n',cube_footer)

     cube_definition

}