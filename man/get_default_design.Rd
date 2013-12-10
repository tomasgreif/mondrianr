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
  Character string consisting of \cr \code{clean_name} \tab
  User-friendly column name. \cr }
}
\examples{
mtcars2 <- mtcars
mtcars2$id <- seq(1:nrow(mtcars2))
mtcars2$vs <- as.integer(mtcars2$vs)
mtcars2$am <- as.integer(mtcars2$am)
mtcars2$gear <- as.integer(mtcars2$gear)
mtcars2$carb <- as.integer(mtcars2$carb)
get_default_design('R',get_table_design('R','mtcars2'),primary_key='id')
}

