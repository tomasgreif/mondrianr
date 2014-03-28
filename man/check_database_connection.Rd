\name{check_database_connection}
\alias{check_database_connection}
\title{Check database existence}
\usage{
check_database_connection(engine, con)
}
\arguments{
  \item{engine}{See function \code{\link{create_schema}}
  for details.}

  \item{con}{See function \code{\link{create_schema}} for
  details.}
}
\description{
Checks if database exists for given data engine and
connection. Function returns \code{TRUE} when database
exists and \code{FALSE} otherwise.
}
\examples{
\dontrun{
check_database_connection('PostgreSQL',c('usr','pwd','db','host','port'))
}
}

