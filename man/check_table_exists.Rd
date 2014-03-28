\name{check_table_exists}
\alias{check_table_exists}
\title{Check table existence}
\usage{
check_table_exists(engine, table, con = NA, debug = FALSE)
}
\arguments{
  \item{engine}{See function \code{\link{create_schema}}
  for details.}

  \item{table}{See function \code{\link{create_schema}} for
  details.}

  \item{con}{Required for PostgreSQL. See function
  \code{\link{create_schema}} for details.}

  \item{debug}{See function \code{\link{create_schema}} for
  details.}
}
\description{
Checks if table exists for given data engine and optional
connection. Function returns \code{TRUE} when table exists
and \code{FALSE} otherwise. This function does not check
that table contains any data or columns at all.
}
\examples{
check_table_exists('R','iris')

\dontrun{
check_table_exists('PostgreSQL','table',c('usr','pwd','db','host','port') }
}

