\name{create_schema}
\alias{create_schema}
\title{Create OLAP Schema}
\usage{
  create_schema(engine, table_design, primary_key,
    numeric_aggregators = NA, date_aggregators = NA, table,
    schema, time_table, time_schema, dimension_data_types,
    numeric_data_types)

  create_schema(engine, table_design, primary_key,
    numeric_aggregators = NA, date_aggregators = NA, table,
    schema, time_table, time_schema, dimension_data_types,
    numeric_data_types)
}
\arguments{
  \item{engine}{Name of database engine. Currently
  supports: \code{R, PostgreSQL}}

  \item{table_design}{Data frame with column names and data
  types. Passed from other functions}

  \item{primary_key}{Passed from other functions}

  \item{numeric_aggregators}{Passed from other functions}

  \item{date_aggregators}{Passed from other functions}

  \item{table}{Passed from other functions}

  \item{schema}{Passed from other functions}

  \item{time_table}{Passed from other functions}

  \item{time_schema}{Passed from other functions}

  \item{dimension_data_types}{Passed from other functions}

  \item{numeric_data_types}{Passed from other functions}

  \item{engine}{Name of database engine. Currently
  supports: \code{R, PostgreSQL}}

  \item{table_design}{Data frame with column names and data
  types. Passed from other functions}

  \item{primary_key}{Passed from other functions}

  \item{numeric_aggregators}{Passed from other functions}

  \item{date_aggregators}{Passed from other functions}

  \item{table}{Passed from other functions}

  \item{schema}{Passed from other functions}

  \item{time_table}{Passed from other functions}

  \item{time_schema}{Passed from other functions}

  \item{dimension_data_types}{Passed from other functions}

  \item{numeric_data_types}{Passed from other functions}
}
\description{
  Create cube content is internal function of \code{olapr}
  package. Based on inputs, it will create correct cube for
  chosen database engine.

  Create cube content is internal function of \code{olapr}
  package. Based on inputs, it will create correct cube for
  chosen database engine.
}

