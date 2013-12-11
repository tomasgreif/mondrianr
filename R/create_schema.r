#' Create Mondrian schema
#'
#' This function returns XML definition of Mondrian schema. Optionally returns file with data source definition 
#' for Saiku.
#'
#' @param engine Data engine. Valid options are: \code{R}, \code{PostgreSQL}. If \code{PostgreSQL} is used then
#' \code{con} is required. Required.
#' @param table Table for which schema should be created. In Mondrian terminology, this is fact table
#' @param primary_key Primary key of \code{table}. Has to be unique. Required
#' @param con Connection to PostgreSQL database. Character vector of exactly five (5) elements: user, password,
#' database, host, port. Required for PostgreSQL
#' @param dimension Rule to modify inclusion/exclusion of columns in \code{table} as dimenions. Has to be defined
#' as valid SQL \code{CASE} statement. Optional
#' @param aggregator Rule to modify inclusion/exclusion of aggregators for columns in \code{table}. Has to be defined
#' as valid SQL \code{CASE} statement and always return string with exactly six (6) digits. See \code{\link{get_default_mapping}}
#' to understand in what are aggregators defined. Optional
#' @param schema_dest Path to file where Mondrian schema will be stored. Has to include file name.
#' @param data_source_dest Path to file where data source definition for Saiku will be stored. Has to include
#' file name. Optional
#' @param time_table Name of table with time dimension. If PostgreSQL than this should be name of existing table
#' with columns \code{time_date, year_number, quarter_number, month_number}. If R than any name can be used as
#' time dimension is created dynamically. Optional. If not used than date columns will be treated as generic
#' dimension. (ToDo: Currently, if you do not use any name for R engine, you will get integers instead of dates
#'in Saiku, because RSQLite stores dates as integers. To be fixed in future version.)
#' @param debug Print additional information useful for debugging.
#' @export 
#' 
create_schema <- function(engine, table, primary_key, con, dimension=NA, aggregator=NA,schema_dest=NA,data_source_dest=NA,
                           time_table=NA,debug=FALSE) {

  prepare_infrastructure(engine=engine, table=table, time_table=time_table, debug=debug)

  table_design    <- get_table_design(engine=engine,table=table,con=con,debug=debug)
  default_design  <- get_default_design(engine=engine, table_design=table_design,primary_key=primary_key,debug=debug)
  final_design    <- get_final_design(engine=engine,default_design=default_design,dimension=dimension,aggregator=aggregator, debug=debug)

  # Generate XML for generic dimensions  
  dimension_xml <- get_generic_dimension_xml(engine=engine, final_design=final_design, table=table, time_table=time_table, primary_key=primary_key, debug=debug)

  # Generate XML for time dimensions
  time_dimension_xml <- get_time_dimension_xml(engine=engine,time_table=time_table, final_design=final_design,con=con, debug=debug)

  # Generate measures for numeric variables
  measure <- get_measure(final_design, debug=debug)

  # Generate XML for measures
  measure_xml <- get_measure_xml(measure,debug=debug)

  # Finalize schema
  schema_definition <- paste0(get_header(engine, table),'\n', time_dimension_xml, '\n', dimension_xml,'\n\n',measure_xml,'\n',get_footer())
  if(debug) cat('Schema defined. \n')

  # Generate connection file and write to specified connection  
  if(!is.na(data_source_dest)) {
    data_source_definition <- get_data_source_definition(engine=engine,table,schema_dest=schema_dest,con=con)  

    writeLines(data_source_definition, con=data_source_dest)
    if(debug) cat('Data source written to destination. \n')
  }
  
  # Write Cube
  writeLines(schema_definition, con=schema_dest)
  if(debug) cat('Cube written to destination. \n')
  }