\name{parse_table_name}
\alias{parse_table_name}
\title{Parse table name}
\usage{
  parse_table_name(engine = NA, table = NA, debug = FALSE)
}
\arguments{
  \item{engine}{Data engine. See function
  \code{\link{create_schema}} for details.}

  \item{table}{Table name to be parsed.}

  \item{debug}{Print additional information useful for
  debugging.}
}
\description{
  This function returns parsed table name. Given table name
  as a string, it always returnes vector with exactly two
  elements. First element is schema, second element is
  table name. When schema is not defined then first element
  is \code{NA}
}
\examples{
parse_table_name(engine='PostgreSQL',table='public.test')
parse_table_name(engine='PostgreSQL',table='my_schema.test')
parse_table_name(engine='PostgreSQL',table='test')
parse_table_name(engine='R',table='iris')
}

