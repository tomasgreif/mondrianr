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
get_data_source_definition('R','mtcars','~/schema.xml')
get_data_source_definition('PostgreSQL','big_portfolio.xml','~/schema.xml',c('usr','pwd','db','host','port'))
mtcars2 <- mtcars
mtcars2$id <- seq(1:nrow(mtcars2))
mtcars2$vs <- as.integer(mtcars2$vs)
mtcars2$am <- as.integer(mtcars2$am)
mtcars2$gear <- as.integer(mtcars2$gear)
mtcars2$carb <- as.integer(mtcars2$carb)
measure <- get_measure(get_final_design('R',get_default_design('R',get_table_design('R','mtcars2'),primary_key='id')))
get_measure_xml(measure)
}

