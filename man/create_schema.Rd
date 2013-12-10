\name{create_schema}
\alias{create_schema}
\title{Create Mondrian schema}
\usage{
  create_schema(engine, table, primary_key, con,
    dimension = NA, aggregator = NA, schema_dest = NA,
    data_source_dest = NA, time_table = NA, debug = FALSE)
}
\arguments{
  \item{engine}{Data engine. Valid options are: \code{R},
  \code{PostgreSQL}. If \code{PostgreSQL} is used then
  \code{con} is required. Required.}

  \item{table}{Table for which schema should be created. In
  Mondrian terminology, this is fact table}

  \item{primary_key}{Primary key of \code{table}. Has to be
  unique. Required}

  \item{con}{Connection to PostgreSQL database. Character
  vector of exactly five (5) elements: user, password,
  database, host, port. Required for PostgreSQL}

  \item{dimension}{Rule to modify inclusion/exclusion of
  columns in \code{table} as dimenions. Has to be defined
  as valid SQL \code{CASE} statement. Optional}

  \item{aggregator}{Rule to modify inclusion/exclusion of
  aggregators for columns in \code{table}. Has to be
  defined as valid SQL \code{CASE} statement and always
  return string with exactly six (6) digits. See
  \code{\link{get_default_mapping}} to understand in what
  are aggregators defined. Optional}

  \item{schema_dest}{Path to file where Mondrian schema
  will be stored. Has to include file name.}

  \item{data_source_dest}{Path to file where data source
  definition for Saiku will be stored. Has to include file
  name. Optional}

  \item{time_table}{Name of table with time dimension. If
  PostgreSQL than this should be name of existing table
  with columns \code{time_date, year_number,
  quarter_number, month_number}. If R than any name can be
  used as time dimension is created dynamically. Required.}

  \item{debug}{Print additional information useful for
  debugging.}
}
\description{
  This function returns XML definition of Mondrian schema.
  Optionally returns file with data source definition for
  Saiku.
}

