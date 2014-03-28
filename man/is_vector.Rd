\name{is_vector}
\alias{is_vector}
\title{Check if argument is vector with given properties}
\usage{
is_vector(vector, length = 1, mode = "character")
}
\arguments{
  \item{vector}{Input object.}

  \item{length}{Required length of vector.}

  \item{mode}{Required mode of vector}
}
\description{
Checks if argument is vector with given length and mode.
Used to control arguments. Returns error when object does
not match given properties.
}
\examples{
is_vector(c(1:9),9,'numeric')
is_vector('a')
is_vector(TRUE,mode='logical')

\dontrun{
is_vector(TRUE,mode='character')
is_vector(c('a','b'),length=3)
is_vector(iris)}
}

