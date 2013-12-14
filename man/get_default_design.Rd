\name{get_default_design}
\alias{get_default_design}
\title{Get default design}
\usage{
  get_default_design(engine, table_design, primary_key,
    debug = FALSE)
}
\arguments{
  \item{engine}{Data engine. See function
  \code{\link{create_schema}} for details.}

  \item{table_design}{Table design as returned by
  \code{\link{get_table_design}}}

  \item{primary_key}{Primary key. See function
  \code{\link{create_schema}} for details.}

  \item{debug}{Print additional information useful for
  debugging.}
}
\description{
  Combines outputs from default mapping (see
  \code{\link{get_default_mapping}})) and table layout (see
  \code{\link{get_table_design}}).
}
\details{
  If successful, dataset with the following columns is
  returned: \tabular{ll}{ Column \tab Description\cr
  \code{name} \tab Column name \cr \code{class} \tab See
  \code{\link{get_default_mapping}} \cr \code{type} \tab
  See \code{\link{get_default_mapping}} \cr
  \code{dimension} \tab See
  \code{\link{get_default_mapping}} \cr \code{aggregator}
  \tab Defines default aggregators for given data type.
  Character string consisting of exactly 6 digits.\cr
  \code{is_primary_key} \tab Flag indicating column with
  primary key. \code{1} if primary key, \code{0} otherwise.
  \cr \code{clean_name} \tab User-friendly column name. \cr
  }
}
\examples{
get_default_design('R',get_table_design('R','german_credit'),primary_key='id')
}

