\name{create_schema_postgresql}
\alias{create_schema_postgresql}
\title{Create OLAP cube definition based on PostgreSQL table}
\usage{
  create_schema_postgresql(table, schema = "public",
    primary_key, time_table, time_schema, schema_dest,
    data_source_dest, con, numeric_aggregators = NA,
    date_aggregators = NA,
    dimension_data_types = c("text", "character varying", "smallint", "integer"),
    numeric_data_types = c("numeric", "integer", "smallint", "bigint"))
}
\arguments{
  \item{table}{Name of PostgreSQL table}

  \item{schema}{Name of schema where table is stored.
  Default value is \code{public}}

  \item{primary_key}{Name of column with primary key in
  table specified in \code{table}. Primary key is not
  strictly required, however it is necesarry that values in
  specified column are unique.}

  \item{time_table}{Name of the time dimension table}

  \item{time_schema}{Name of PostgreSQL schema where time
  dimension is stored}

  \item{schema_dest}{Where XML file with cube definition
  should be written. Has to include file name. Defaults to
  current working directory}

  \item{data_source_dest}{Where data source definition
  should be written. Has to include file name. This
  definition is Saiku specific}

  \item{con}{Vector specifying connection to PostgreSQL
  database. Has to have exactly five elements in this order
  - user name, password, database name, hostname, port.}

  \item{numeric_aggregators}{Vector defining what Mondrian
  aggregators will be used for measures. Can be any
  combination of
  \code{'sum','count','min','max','avg','distinct-count'}.
  Defaults to all aggregators}

  \item{date_aggregators}{Vector defining what Mondrian
  aggregators will be used for date columns. Can be any
  combination of
  \code{'sum','count','min','max','avg','distinct-count'}.
  Defaults to \code{'count'}.}

  \item{dimension_data_types}{Vector definining what data
  types will be used to create dimensions. Defaults to
  \code{'text','character varying','smallint','integer'}.
  Date columns will be included automatically even if not
  specified.}

  \item{numeric_data_types}{Vector defining what data types
  will be in measures. Defaults to
  \code{'numeric','integer','smallint','bigint'}. Date
  columns will be included automatically.}
}
\description{
  Create OLAP cube definition based on PostgreSQL table
}

