\name{get_data_source_definition}
\alias{get_data_source_definition}
\title{Get data source definition for Saiku}
\usage{
  get_data_source_definition(engine, table, schema_dest,
    con, debug = FALSE)
}
\arguments{
  \item{engine}{Data engine. See function
  \code{\link{create_schema}} for details.}

  \item{table}{Table for which connection file should be
  created (used to generate name).}

  \item{schema_dest}{Where schema definition will be
  stored. Valid path, including file name with schema.}

  \item{con}{Connection. See \code{\link{create_schema}}
  for details.}

  \item{debug}{Print additional information useful for
  debugging.}
}
\description{
  Returns valid data source definition for Saiku
}
\examples{
\dontrun{
get_data_source_definition('R','mtcars','~/schema.xml')
get_data_source_definition('PostgreSQL','big_portfolio.xml','~/schema.xml',c('usr','pwd','db','host','port'))
}
}

