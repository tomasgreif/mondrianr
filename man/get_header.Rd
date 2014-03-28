\name{get_header}
\alias{get_header}
\title{Get schema header}
\usage{
get_header(engine, table, debug = NA)
}
\arguments{
  \item{engine}{Data engine. See function
  \code{\link{create_schema}} for details.}

  \item{table}{Table for which header should be returned.}

  \item{debug}{Print additional information useful for
  debugging.}
}
\description{
Returns schema header for given data engine and table.
}
\examples{
get_header('R','iris')
get_header('PostgreSQL','schema.table')
}

