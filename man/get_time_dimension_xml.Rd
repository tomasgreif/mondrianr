\name{get_time_dimension_xml}
\alias{get_time_dimension_xml}
\title{Get XML for time dimension}
\usage{
get_time_dimension_xml(engine, time_table = NA, final_design, con,
  debug = FALSE)
}
\arguments{
  \item{engine}{Data engine. See function
  \code{\link{create_schema}} for details.}

  \item{final_design}{Final schema design. See function
  \code{\link{get_final_design}} for details.}

  \item{time_table}{Table with time dimension. See function
  \code{\link{create_schema}} for details.}

  \item{con}{Connection. See function
  \code{\link{create_schema}} for details.}

  \item{debug}{Print additional information useful for
  debugging.}
}
\description{
Returns XML for time dimensions. Creates XML that will be
used in Mondrian schema.
}

