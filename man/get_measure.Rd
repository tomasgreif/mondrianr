\name{get_measure}
\alias{get_measure}
\title{Get measure}
\usage{
  get_measure(final_design, debug = FALSE)
}
\arguments{
  \item{final_design}{Final schema design. See function
  \code{\link{get_final_design}} for details.}

  \item{debug}{Print additional information useful for
  debugging.}
}
\description{
  Takes \code{final_design} as input and returns data frame
  with one row for every enabled aggregator.
}
\details{
  If successful, dataset with the following columns is
  returned: \tabular{ll}{ Column \tab Description\cr
  \code{name} \tab Class of data type.\cr \code{schema}
  \tab Database schema. If not specified for given data
  engine than NA.\cr \code{aggregator_name} \tab Aggregator
  name as defined in Mondrian schema specification.\cr
  \code{measure_name} \tab User-friendly name.\cr }
}
\examples{
get_measure(get_final_design('R',get_default_design('R',get_table_design('R','german_credit'),primary_key='id')))
}

