\name{check_table_exists}
\alias{check_table_exists}
\title{Check table existence}
\usage{
  check_table_exists(engine, table, con, debug = FALSE)
}
\arguments{
  \item{engine}{See function \code{\link{create_schema}}
  for details.}

  \item{table}{See function \code{\link{create_schema}} for
  details.}

  \item{con}{See function \code{\link{create_schema}} for
  details.}

  \item{debug}{See function \code{\link{create_schema}} for
  details.}
}
\description{
  Checks if table exists for given data engine and
  connection. Function returns \code{TRUE} when table
  exists and \code{FALSE} otherwise.
}

