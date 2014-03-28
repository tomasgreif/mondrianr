\name{create_time_dimension_postgresql}
\alias{create_time_dimension_postgresql}
\title{Create time dimension for PostgreSQL}
\usage{
create_time_dimension_postgresql(engine, final_design, table, time_table,
  con = NA, debug = FALSE, force = FALSE)
}
\arguments{
  \item{engine}{Data engine. See function
  \code{\link{create_schema}} for details.}

  \item{final_design}{See function
  \code{\link{get_final_design}} for details.}

  \item{table}{Table with original data}

  \item{time_table}{Name of time dimension (fully qualified
  table name)}

  \item{con}{See function \code{\link{create_schema}} for
  details.}

  \item{debug}{See function \code{\link{create_schema}} for
  details.}

  \item{force}{If \code{TRUE} then existing table will be
  removed. Use with care! Default is \code{FALSE}}
}
\description{
This function creates time dimension in PostgreSQL. for
given data engine.
}
\examples{
\dontrun{
create_time_dimension_postgresql('PostgreSQL',final_design,'some_table','dw.timetest',
     con=c('user','password','database','host','port'),force=TRUE)
}
}

