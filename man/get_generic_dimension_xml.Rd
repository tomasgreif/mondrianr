\name{get_generic_dimension_xml}
\alias{get_generic_dimension_xml}
\title{Get XML for generic dimension}
\usage{
get_generic_dimension_xml(engine, final_design, table, time_table = NA,
  primary_key, debug = FALSE)
}
\arguments{
  \item{engine}{Data engine. See function
  \code{\link{create_schema}} for details.}

  \item{final_design}{Final schema design. See function
  \code{\link{get_final_design}} for details.}

  \item{table}{Table for which connection file should be
  created (used to generate name).}

  \item{time_table}{Time table. See function
  \code{\link{create_schema}} for details.}

  \item{primary_key}{Primary key of \code{table}.}

  \item{debug}{Print additional information useful for
  debugging.}
}
\description{
Returns XML for generic dimensions Creates XML that will be
used in Mondrian schema. Generic dimensions are currently
all columns where dimension is \code{TRUE} in
\code{final_design} and column is not of \code{class} date.
}
\examples{
final_design <- get_final_design('R',
 get_default_design('R',get_table_design('R','german_credit'),primary_key='id'))
get_generic_dimension_xml('R',final_design, 'german_credit',primary_key='id')
}

