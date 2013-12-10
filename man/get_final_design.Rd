\name{get_final_design}
\alias{get_final_design}
\title{Get final design}
\usage{
  get_final_design(engine, default_design, dimension = NA,
    aggregator = NA, debug = FALSE)
}
\arguments{
  \item{engine}{Data engine. See function
  \code{\link{create_schema}} for details.}

  \item{default_design}{See function
  \code{\link{get_default_design}} for details.}

  \item{dimension}{Rule to include/exclude dimensions. See
  function \code{\link{create_schema}} for details.}

  \item{aggregator}{Rule to include/exclude aggregators for
  columns. See function \code{\link{create_schema}} for
  details.}

  \item{debug}{Print additional information useful for
  debugging.}
}
\description{
  Takes \code{default_design} as input and modifies it
  accordingly.
}

