\name{get_table_design}
\alias{get_table_design}
\title{Get table design}
\usage{
  get_table_design(engine = NA, table = NA, con = NA,
    debug = FALSE)
}
\arguments{
  \item{engine}{Data engine. See function
  \code{\link{create_schema}} for details.}

  \item{table}{Table to be analyzed.}

  \item{con}{Connection. See function
  \code{\link{create_schema}} for details.}

  \item{debug}{Print additional information useful for
  debugging.}
}
\description{
  This function returns data frame with structure of
  specified table for given data engine. If successful,
  dataset with the following columns is returned:
  \tabular{ll}{ Column \tab Description\cr \code{name} \tab
  Class of data type.\cr \code{type} \tab Data type as
  defined by data engine. This is used to map data type to
  default schema properties as defined in
  \code{\link{get_default_mapping}}.\cr \code{schema} \tab
  Database schema. If not specified for given data engine
  than NA }
}
\examples{
get_table_design(engine='R', table='iris')

\dontrun{
get_table_design(engine='PostgreSQL',
 table='public.big_portfolio',con=c('usr','pwd','db','host','port'))}
}

