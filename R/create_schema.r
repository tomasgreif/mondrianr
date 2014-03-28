#' Create Mondrian schema
#'
#' This function returns XML definition of Mondrian schema. Optionally returns file with data source definition 
#' for Saiku.
#' 
#' Arguments \code{dimension} and \code{aggregator} are required to be valid SQL \code{CASE} statement. Case statement is similar to 
#' \code{if} statement available in many languages. Basic structure is: \code{CASE WHEN <CONDITION> THEN <VALUE> [[WHEN <CONDITION> THEN <VALUE>] ELSE <VALUE>] END}.
#' Both \code{dimension} and \code{aggregator} arguments are used to modify content of data frame with default design 
#' (see \code{\link{get_default_design}} for details). This means that you can use columns \code{name, class, type, dimension, aggregator}
#' in case statement. 
#' Examples for dimension: 
#' \itemize{
#'  \item For numeric data types do not create dimenions, otherwise do create dimensions: \code{case when class='numeric' then 0 else 1 end}
#'  \item For columns a, b create dimenions and for other columns keep default value: \code{case when name in('a','b') then 1 else dimension end}
#'  }
#'  
#'  Examples for aggregator:
#'  \itemize{
#'    \item Only count aggregator for non-numeric columns and default for other columns: \code{case when class <> 'numeric' then '010000' else aggregator end}
#'    \item No aggregator for date and text columns and count for all others: \code{case when class in('text','numeric') then '000000' else '010000' end}
#'    \item No aggregator for dimensions, default for others: \code{case when dimension=1 then '000000' else aggregator end}
#'    \item No aggregators at all (only count for PK will be displayed): \code{000000}
#'  }
#'  
#' @param engine Data engine. Valid options are: \code{R}, \code{PostgreSQL}. If \code{PostgreSQL} is used then
#' \code{con} is required. Required.
#' @param table Table for which schema should be created. In Mondrian terminology, this is fact table
#' @param primary_key Primary key of \code{table}. Has to be unique. Required
#' @param con Connection to PostgreSQL database. Character vector of exactly five (5) elements: \code{'user', 'password',
#' 'database', 'host', 'port'}. Required for PostgreSQL.
#' @param dimension Rule to modify inclusion/exclusion of columns in \code{table} as dimenions. Has to be defined
#' as valid SQL \code{CASE} statement. Optional.
#' @param aggregator Rule to modify inclusion/exclusion of aggregators for columns in \code{table}. Has to be defined
#' as valid SQL \code{CASE} statement and always return string with exactly six (6) digits. If same aggregetors should be used for all variables then string with exactly 6 digits
#' can be used instead of case statement. If digit is \code{1} than aggregator 
#' is enabled. If \code{0} then aggregator is disabled.  Aggregators are defined in the following 
#' order: \code{average, count, distinct-count, maximum, minimum, sum}. Argument is evaluated after argument \code{dimension}. Optional
#' @param schema_dest Path to file where Mondrian schema will be stored. Has to include file name.
#' @param data_source_dest Path to file where data source definition for Saiku will be stored. Has to include
#' file name. Optional.
#' @param calculated_member List defining additional calculated members (measures calculated on results of other measures). Unlimited number of 
#' calculated members can be created. Every component in list has to have exactly three elements:
#' \itemize{
#'  \item Name of calculated member. This will be displayed as user-friendly name in Mondrian compatible analytical tools
#'  \item Formula. Valid MDX expression for calculated member.
#'  \item Format. Format to apply. If default should be used then use \code{NA}.
#' }
#' Optional.
#' @param time_table Name of table with time dimension. If PostgreSQL than this should be name of existing table
#' with columns \code{time_date, year_number, quarter_number, month_number}. If R than any name can be used as
#' time dimension is created dynamically. Optional. If not used than date columns will be treated as generic
#' dimension.
#' @param security_type Type of security for Saiku. Currently only one2one is supported. Using this makes sense only when \code{security_roles_xml} is used. Optional.
#' @param security_roles Vector defining what roles will have access to cube. Meaningful only when \code{security_typ} is set to \code{one2one}. When only one role
#' is given than this will be used to GRANT all rights to cube to this role. When two roles are given than the first will be used to GRANT all rights
#' to cube and the second will be used to DENY access to everything. Optional.
#' @param debug Print additional information useful for debugging.
#' @examples
#' 
#' # Get some valid destinations
#' schema_dest <- paste0(getwd(),'/','test.xml')
#' data_source_dest <- paste0(getwd(),'/','test')
#'
#' # Simple schema, no time dimension, no data source definition
#' create_schema(engine='R',table='big_portfolio',primary_key='id', schema_dest=schema_dest)
#' 
#' # Do this again, but print additional debug messages.
#' create_schema(engine='R',table='big_portfolio',primary_key='id', 
#'               schema_dest=schema_dest, debug=TRUE)
#' 
#' # Create schema with time dimension
#' create_schema(engine='R',table='big_portfolio',primary_key='id', time_table = 'any',
#'               schema_dest=schema_dest, data_source_dest=data_source_dest)
#'               
#' # Modify default dimensions (create dimension only for factors)
#' create_schema(engine='R',table='big_portfolio',primary_key='id', time_table = 'any',
#'               schema_dest=schema_dest, dimension="case when type='factor' then 1 else 0 end")
#'               
#' # Modify default dimensions (create dimension only for specified columns)
#' create_schema(engine='R',table='big_portfolio',primary_key='id', time_table = 'any',
#'               schema_dest=schema_dest, 
#'               dimension="case when name in('product','region') then 1 else 0 end")
#'               
#' # Modify aggregators - enable all aggregators for numeric columns, keep default for others
#' create_schema(engine='R',table='big_portfolio',primary_key='id', time_table = 'any', 
#'               schema_dest=schema_dest, 
#'               aggregator="case when type='numeric' then '111111' else aggregator end")
#' 
#' # Create schema with calculated members
#' create_schema(engine='R',table='big_portfolio',primary_key='id',time_table = 'any_name',
#'              schema_dest=schema_dest, data_source_dest=data_source_dest,
#'              calculated_member=list(
#'                 c('Not repaid pct',
#'                   '[Measures].[Current balance-Sum]/[Measures].[Original balance-Sum]',NA),
#'                 c('Repaid pct',
#'                   '[Measures].[Current balance-Sum]/[Measures].[Original balance-Sum]','##.00%')
#'             ))
#'             
#' \dontrun{
#' # Complex example for PostgreSQL. tmp_time has to exist
#' create_schema(engine='PostgreSQL',table='big_portfolio',primary_key='id',time_table = 'tmp_time',
#'              schema_dest=schema_dest, data_source_dest=data_source_dest,
#'              con=c('usr','pwd','db','host','port'),
#'              calculated_member=list(
#'                 c('Not repaid pct',
#'                   '[Measures].[Current balance-Sum]/[Measures].[Original balance-Sum]',NA),
#'                 c('Repaid pct',
#'                   '[Measures].[Current balance-Sum]/[Measures].[Original balance-Sum]','##.00%')
#'             ))
#' }                                  
#' @export 
#' 
create_schema <- function(engine=NA, table=NA, primary_key=NA, con=NA, dimension=NA, aggregator=NA,schema_dest=NA,data_source_dest=NA,
                           time_table=NA,calculated_member=NA, security_type=NA, security_roles=NA, debug=FALSE) {

  check_inputs(engine=engine, table=table, primary_key=primary_key, con=con, dimension=dimension, aggregator=aggregator,
               schema_dest=schema_dest,data_source_dest=data_source_dest,time_table=time_table,security_type=security_type,
               security_roles=security_roles,debug=debug)
  
  prepare_infrastructure(engine=engine, table=table, time_table=time_table, debug=debug)

  # Create schema design
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
  
  # Generate XML for calculated members
  calculated_member_xml <- get_calculated_member_xml(engine, calculated_member, debug=debug)

  # Generate connection file and write to specified connection  
  create_data_source_definition(engine=engine, table=table, schema_dest=schema_dest, data_source_dest, con=con, security_type=security_type, 
                                debug=debug)  

  # Finalize schema and write it to destination
  create_schema_xml(engine=engine,table=table,time_dimension_xml=time_dimension_xml,dimension_xml=dimension_xml,measure_xml=measure_xml,
                    calculated_member_xml = calculated_member_xml, security_roles = security_roles,
                    schema_dest=schema_dest,debug=debug)
  
  cat('Process successfuly finished. \n')
  }
