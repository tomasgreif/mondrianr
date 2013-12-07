#' Create OLAP cube definition based on PostgreSQL table
#' 
#' @param table Name of PostgreSQL table
#' @param schema Name of schema where table is stored. Default value is \code{public}
#' @param primary_key Name of column with primary key in table specified in \code{table}. Primary key is not strictly required, however it is necesarry that 
#' values in specified column are unique.
#' @param time_table Name of the time dimension table
#' @param time_schema Name of PostgreSQL schema where time dimension is stored
#' @param schema_dest Where XML file with cube definition should be written. Has to include file name. Defaults to current working directory
#' @param data_source_dest Where data source definition should be written. Has to include file name. This definition is Saiku specific
#' @param con Vector specifying connection to PostgreSQL database. Has to have exactly five elements in this order - user name, password, database name, hostname, port.
#' @param numeric_aggregators Vector defining what Mondrian aggregators will be used for measures. Can be any combination of \code{'sum','count','min','max','avg','distinct-count'}.
#' Defaults to all aggregators
#' @param date_aggregators Vector defining what Mondrian aggregators will be used for date columns. Can be any combination of \code{'sum','count','min','max','avg','distinct-count'}.
#' Defaults to \code{'count'}.
#' @param dimension_data_types Vector definining what data types will be used to create dimensions. Defaults to \code{'text','character varying','smallint','integer'}. Date columns will be
#' included automatically even if not specified.
#' @param numeric_data_types Vector defining what data types will be in measures. Defaults to \code{'numeric','integer','smallint','bigint'}. Date columns will be included automatically.
#' @export 

create_schema_postgresql <- function(table,schema='public',primary_key, time_table, time_schema,schema_dest,
                                   data_source_dest, con, numeric_aggregators=NA, date_aggregators=NA,
                                   dimension_data_types=c('text', 'character varying', 'smallint', 'integer'),
                                   numeric_data_types=c('numeric', 'integer', 'smallint', 'bigint')) {
          
     
# --------- Define connection
     #require(sqldf)

     options(sqldf.RPostgreSQL.user = con[1], 
        sqldf.RPostgreSQL.password = con[2],
        sqldf.RPostgreSQL.dbname = con[3],
        sqldf.RPostgreSQL.host = con[4], 
        sqldf.RPostgreSQL.port = con[5])

# -------- Get data from information schema
  table_design <- sqldf(paste0("select column_name, data_type, ordinal_position from information_schema.columns where table_name = '",
                                 table,"' and table_schema='",schema,"'"),drv='PostgreSQL')
  time_table_design <-  sqldf(paste0("select column_name, data_type, ordinal_position from information_schema.columns where table_name = '",
                                      time_table,"' and table_schema='",time_schema,"'"),drv='PostgreSQL')

# -------- Do some basic error checking
     if(nrow(table_design)==0) {
          stop("Table defined as table does not exists (using specified connection) or does not contain any columns. Fix paramaters and run command again.")
     }

     if(nrow(time_table_design)==0) {
          stop("Table defined as time_table does not exists (using specified connection) or does not contain any columns. Fix paramaters and run command again.")
     }     
     
     if(!(primary_key %in% table_design[,1])) {
           stop("Column defined as primary key does not exist in specified table. Fix parameters and run command again.")
      }
     
# -------- Get lists of dimensions, time dimensions, measures
     schema_definition <- create_schema(engine='PostgreSQL',
                                         primary_key=primary_key,
                                         table=table,
                                         schema=schema,
                                         table_design=table_design,
                                         numeric_aggregators=numeric_aggregators,
                                         date_aggregators=date_aggregators,
                                         time_table=time_table,
                                         time_schema=time_schema,
                                         dimension_data_types=dimension_data_types,
                                         numeric_data_types=numeric_data_types)

     # --------- Generate connection file
     
     data_source_definition <- paste0(
          'type=OLAP
      name=',schema,'.',table,'
      driver=mondrian.olap4j.MondrianOlap4jDriver
      location=jdbc:mondrian:Jdbc=jdbc:postgresql://', con[4],':', con[5],'/', con[3],'; \\
      Catalog=', schema_dest,';JdbcDrivers=org.postgresql.Driver;
      username=', con[1],'
      password=', con[2]
     )
     
     # ---------- Write results to file
     
     # Write Cube
     writeLines(schema_definition, con=schema_dest)
     # Write Data source
     writeLines(data_source_definition, con=data_source_dest)
     
}     