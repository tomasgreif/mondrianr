\name{get_default_mapping}
\alias{get_default_mapping}
\title{Get default mapping}
\usage{
  get_default_mapping(engine = NA, debug = FALSE)
}
\arguments{
  \item{engine}{Data engine. See function
  \code{\link{create_schema}} for details.}

  \item{debug}{Print additional information useful for
  debugging.}
}
\description{
  This function returns data frame with default mapping of
  data types to schema properties for given data engine.
}
\details{
  If successful, dataset with the following columns is
  returned: \tabular{ll}{ Column \tab Description\cr
  \code{class} \tab Class of data type. Raw data types are
  aggregated into high-level groups. For example in
  PostgreSQL, data types \code{smallint, integer, bigint}
  have class \code{integer}. This grouping can be used when
  using argument \code{aggregator} in function
  \code{\link{create_schema}}.\cr \code{type} \tab Data
  type as defined by data engine.\cr \code{dimension} \tab
  If \code{TRUE} than columns with given data type will be
  used to create dimensions.\cr \code{aggregator} \tab
  Defines default aggregators for given data type.
  Character string consisting of exactly six digits. If
  \code{1} than aggregator is enabled. If \code{0} then
  aggregator is disabled. Aggregators are defined in the
  following order: \code{average, count, distinct-count,
  maximum, minimum, sum}.\cr } Examples for column
  \code{aggregator}: \itemize{ \item \code{111111} - All
  aggregators enabled \item \code{100001} - Only average
  and sumenabled \item \code{010000} - Only count enabled
  \item \code{000000} - All aggregators disabled - for
  columns with given data type there will be no measure. }
}
\examples{
get_default_mapping('PostgreSQL')
get_default_mapping('R')
get_default_mapping('R', debug=TRUE)
}

