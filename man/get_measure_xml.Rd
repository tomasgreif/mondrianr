\name{get_measure_xml}
\alias{get_measure_xml}
\title{Get XML for measures}
\usage{
  get_measure_xml(measure, debug = FALSE)
}
\arguments{
  \item{measure}{Data frame with measures. See
  \code{\link{get_measure}} for details.}

  \item{debug}{Print additional information useful for
  debugging.}
}
\description{
  Returns XML for measures
}
\details{
  Takes output from function \code{get_measure} and creates
  XML that will be used in Mondrian schema.
}
\examples{
measure <- get_measure(get_final_design('R',get_default_design('R',get_table_design('R','german_credit'),primary_key='id')))
get_measure_xml(measure)
}

