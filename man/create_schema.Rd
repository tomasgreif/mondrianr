\name{create_schema}
\alias{create_schema}
\title{Create Mondrian schema}
\usage{
  create_schema(engine = NA, table = NA, primary_key = NA,
    con = NA, dimension = NA, aggregator = NA,
    schema_dest = NA, data_source_dest = NA,
    time_table = NA, calculated_member = NA, debug = FALSE)
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
  vector of exactly five (5) elements: \code{'user',
  'password', 'database', 'host', 'port'}. Required for
  PostgreSQL.}

  \item{dimension}{Rule to modify inclusion/exclusion of
  columns in \code{table} as dimenions. Has to be defined
  as valid SQL \code{CASE} statement. Optional.}

  \item{aggregator}{Rule to modify inclusion/exclusion of
  aggregators for columns in \code{table}. Has to be
  defined as valid SQL \code{CASE} statement and always
  return string with exactly six (6) digits. If digit is
  \code{1} than aggregator is enabled. If \code{0} then
  aggregator is disabled.  Aggregators are defined in the
  following order: \code{average, count, distinct-count,
  maximum, minimum, sum}. Argument is evaluated after
  argument \code{dimension}. Optional}

  \item{schema_dest}{Path to file where Mondrian schema
  will be stored. Has to include file name.}

  \item{data_source_dest}{Path to file where data source
  definition for Saiku will be stored. Has to include file
  name. Optional.}

  \item{calculated_member}{List defining additional
  calculated members (measures calculated on results of
  other measures). Unlimited number of calculated members
  can be created. Every component in list has to have
  exactly three elements: \itemize{ \item Name of
  calculated member. This will be displayed as
  user-friendly name in Mondrian compatible analytical
  tools \item Formula. Valid MDX expression for calculated
  member.  \item Format. Format to apply. If default should
  be used then use \code{NA}. } Optional.}

  \item{time_table}{Name of table with time dimension. If
  PostgreSQL than this should be name of existing table
  with columns \code{time_date, year_number,
  quarter_number, month_number}. If R than any name can be
  used as time dimension is created dynamically. Optional.
  If not used than date columns will be treated as generic
  dimension.}

  \item{debug}{Print additional information useful for
  debugging.}
}
\description{
  This function returns XML definition of Mondrian schema.
  Optionally returns file with data source definition for
  Saiku.
}
\details{
  Arguments \code{dimension} and \code{aggregator} are
  required to be valid SQL \code{CASE} statement. Case
  statement is similar to \code{if} statement available in
  many languages. Basic structure is: \code{CASE WHEN
  <CONDITION> THEN <VALUE> [[WHEN <CONDITION> THEN <VALUE>]
  ELSE <VALUE>] END}. Both \code{dimension} and
  \code{aggregator} arguments are used to modify content of
  data frame with default design (see
  \code{\link{get_default_design}} for details). This means
  that you can use columns \code{name, class, type,
  dimension, aggregator} in case statement. Examples for
  dimension: \itemize{ \item For numeric data types do not
  create dimenions, otherwise do create dimensions:
  \code{case when class='numeric' then 0 else 1 end} \item
  For columns a, b create dimenions and for other columns
  keep default value: \code{case when name in('a','b') then
  1 else dimension end} }

  Examples for aggregator: \itemize{ \item Only count
  aggregator for non-numeric columns and default for other
  columns: \code{case when class <> 'numeric' then '010000'
  else aggregator end} \item No aggregator for date and
  text columns and count for all others: \code{case when
  class in('text','numeric') then '000000' else '010000'
  end} \item No aggregtor for dimensions, default for
  others: \code{case when dimension=1 then '000000' else
  aggregator end} }
}
\examples{
#----------------
schema_dest <- paste0(getwd(),'/','test.xml')
data_source_dest <- paste0(getwd(),'/','test')

# Simple schema, no time dimension, no data source definition
create_schema(engine='R',table='big_portfolio',primary_key='id', schema_dest=schema_dest)

# Do this again, but print additional debug messages.
create_schema(engine='R',table='big_portfolio',primary_key='id', schema_dest=schema_dest, debug=TRUE)

# Create schema with time dimension
create_schema(engine='R',table='big_portfolio',primary_key='id', time_table = 'any',
              schema_dest=schema_dest, data_source_dest=data_source_dest)

# Modify default dimensions (create dimension only for factors)
create_schema(engine='R',table='big_portfolio',primary_key='id', time_table = 'any',
              schema_dest=schema_dest, dimension="case when type='factor' then 1 else 0 end")

# Modify default dimensions (create dimension only for specified columns)
create_schema(engine='R',table='big_portfolio',primary_key='id', time_table = 'any',
              schema_dest=schema_dest, dimension="case when name in('product','region') then 1 else 0 end")

# Modify aggregators - enable all aggregators for numeric columns, keep default for others
create_schema(engine='R',table='big_portfolio',primary_key='id', time_table = 'any', schema_dest=schema_dest,
              aggregator="case when type='numeric' then '111111' else aggregator end")

# Create schema with calculated members
create_schema(engine='R',table='big_portfolio',primary_key='id',time_table = 'any_name',
             schema_dest=schema_dest, data_source_dest=data_source_dest,
             calculated_member=list(
                c('Not repaid pct','[Measures].[Current balance-Sum]/[Measures].[Original balance-Sum]',NA),
                c('Repaid pct','[Measures].[Current balance-Sum]/[Measures].[Original balance-Sum]','##.00\%')
            ))
}

