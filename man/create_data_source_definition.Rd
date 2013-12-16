\name{create_data_source_definition}
\alias{create_data_source_definition}
\title{Creates data source definition for Saiku}
\usage{
  create_data_source_definition(engine, table, schema_dest,
    data_source_dest, con, debug = FALSE)
}
\arguments{
  \item{engine}{Data engine. See function
  \code{\link{create_schema}} for details.}

  \item{table}{Table for which connection file should be
  created (used to generate name).}

  \item{schema_dest}{Where schema definition will be
  stored. Valid path, including file name with schema.}

  \item{data_source_dest}{Where data source definition file
  should be written.}

  \item{con}{Connection. See \code{\link{create_schema}}
  for details.}

  \item{debug}{Print additional information useful for
  debugging.}
}
\description{
  Returns valid data source definition for Saiku and writes
  it to destination
}
\examples{
\dontrun{
create_data_source_definition('R','mtcars','~/schema.xml','~/data_source')
create_data_source_definition('PostgreSQL','big_portfolio.xml',
 '~/schema.xml','~/data_source',c('usr','pwd','db','host','port'))
}
}

