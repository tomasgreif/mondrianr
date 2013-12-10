\name{prepare_infrastructure}
\alias{prepare_infrastructure}
\title{Prepare infrastructure}
\usage{
  prepare_infrastructure(engine = NA, table = NA,
    time_table = NA, debug = FALSE)
}
\arguments{
  \item{engine}{Data engine. See function
  \code{\link{create_schema}} for details.}

  \item{table}{Table with data. See function
  \code{\link{create_schema}} for details.}

  \item{time_table}{Table with time dimension. See function
  \code{\link{create_schema}} for details.}

  \item{debug}{Print additional information useful for
  debugging.}
}
\description{
  Prepare infrastructure. This includes all steps necessary
  before we start creating schema. For some data engines
  there is nothing to prepare
}

