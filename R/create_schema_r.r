#' Create OLAP cube definition based on R data frame

#' @param data Name of R data frame. Passed as object, not string (correct: \code{data_frame}, wrong: \code{'data_frame'}..
#' @param primary_key Name of column with primary key in table specified in \code{table}. Primary key is not strictly required, however it is necesarry that 
#' values in specified column are unique.
#' @param schema_dest Where XML file with cube definition should be written. Has to include file name. Defaults to current working directory
#' @param data_source_dest Where data source definition should be written. Has to include file name. This definition is Saiku specific
#' @param numeric_aggregators Vector defining what Mondrian aggregators will be used for measures. Can be any combination of \code{'sum','count','min','max','avg','distinct-count'}.
#' Defaults to all aggregators
#' @param date_aggregators Vector defining what Mondrian aggregators will be used for date columns. Can be any combination of \code{'sum','count','min','max','avg','distinct-count'}.
#' Defaults to \code{'count'}.
#' @param dimension_data_types Vector definining what data types will be used to create dimensions. Defaults to \code{'character','integer'}. Date columns will be
#' included automatically even if not specified.
#' @param numeric_data_types Vector defining what data types will be in measures. Defaults to \code{'numeric','integer','double'}. Date columns will be included automatically.
#' @param min_time Start of time dimension. Time dimension is generated dynamically during function execution. Defaults to 2001-01-01.
#' @param max_time End of time dimension. Defaults to 2015-01-01.
#' @export 

create_schema_r <- function(data, primary_key, schema_dest, data_source_dest, numeric_aggregators=NA, date_aggregators=NA, 
                          max_time='2015-01-01', min_time='2001-01-01', dimension_data_types=c('character','integer'), 
                          numeric_data_types=c('numeric','integer','double')) {
     

     # --------- Define connection
     #require(sqldf)
     #require(RSQLite)
     
     file.remove('__this_is_temporary_db__.db')
     sqlite    <- dbDriver("SQLite")
     tmpdb <- dbConnect(sqlite,"__this_is_temporary_db__.db")           
     
     data_name <- deparse(substitute(data))
     table <- data_name
     
     dbWriteTable(tmpdb,data_name,data)
     
     #------ Generate time dimension
     ddiff <- as.integer(as.Date(max_time)) - as.integer(as.Date(min_time))
     tmp_time <- data.frame(time_date = as.Date(min_time) + seq(1:ddiff))
     tmp_time$month_number <- as.integer(format(tmp_time$time_date, "%m"))
     tmp_time$year_number <- as.integer(format(tmp_time$time_date, "%Y"))
     tmp_time$quarter_number[tmp_time$month_number %in% 1:3] <- 1
     tmp_time$quarter_number[tmp_time$month_number %in% 4:6] <- 2
     tmp_time$quarter_number[tmp_time$month_number %in% 7:9] <- 3
     tmp_time$quarter_number[tmp_time$month_number %in% 10:12] <- 4
     time_table <- 'tmp_time'
     dbWriteTable(tmpdb,'tmp_time',tmp_time)     
     
     #------ Disconnect     
     dbDisconnect(tmpdb)     
     
     # -------- Get column types from R data frame
     table_design <- data.frame(type=sapply(data, typeof), datetype=sapply(names(data), function(x) inherits(data[[x]], "Date")))
     table_design$column_name <- row.names(table_design)
     row.names(table_design) <- NULL
     table_design <- sqldf("select column_name, case when datetype then 'date' else type end as data_type from table_design", drv='SQLite')
     
     
     #----------- Get cube
     
     schema_definition <- create_schema(engine='R',
                                         primary_key=primary_key,
                                         table=table,
                                         schema=NA,
                                         table_design=table_design,
                                         numeric_aggregators=numeric_aggregators,
                                         date_aggregators=date_aggregators,
                                         time_table='tmp_time',
                                         time_schema=NA,
                                         dimension_data_types=dimension_data_types,
                                         numeric_data_types=numeric_data_types)
  
     
     # --------- Generate connection file
     
     data_source_definition <- paste0('type=OLAP
                              name=RData 
                              driver=mondrian.olap4j.MondrianOlap4jDriver
                              location=jdbc:mondrian:Jdbc=jdbc:sqlite:',getwd(),'/__this_is_temporary_db__.db; \\
                              Catalog=',schema_dest,';JdbcDrivers=org.sqlite.JDBC 
                              ')
     
     # ---------- Write results to file
     
     # Write Cube
     writeLines(schema_definition,con=schema_dest)
     # Write Data source
     writeLines(data_source_definition,con=data_source_dest)     
     }
