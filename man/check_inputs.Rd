\name{check_inputs}
\alias{check_inputs}
\title{Check inputs}
\usage{
check_inputs(engine = NA, table = NA, primary_key = NA, con = NA,
  dimension = NA, aggregator = NA, schema_dest = NA,
  data_source_dest = NA, time_table = NA, security_type = NA,
  security_roles = NA, debug = FALSE)
}
\arguments{
  \item{engine}{See function \code{\link{create_schema}}
  for details.}

  \item{table}{See function \code{\link{create_schema}} for
  details.}

  \item{primary_key}{See function
  \code{\link{create_schema}} for details.}

  \item{con}{See function \code{\link{create_schema}} for
  details.}

  \item{dimension}{See function \code{\link{create_schema}}
  for details.}

  \item{aggregator}{See function
  \code{\link{create_schema}} for details.}

  \item{schema_dest}{See function
  \code{\link{create_schema}} for details.}

  \item{data_source_dest}{See function
  \code{\link{create_schema}} for details.}

  \item{time_table}{See function
  \code{\link{create_schema}} for details.}

  \item{security_type}{See function
  \code{\link{create_schema}} for details.}

  \item{security_roles}{See function
  \code{\link{create_schema}} for details.}

  \item{debug}{See function \code{\link{create_schema}} for
  details.}
}
\description{
Checks inputs to function \code{\link{create_schema}}.
Error is reported when wrong configuration is found,
otherwise this function returns nothing.
}

