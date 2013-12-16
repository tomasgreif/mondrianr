\name{create_schema_xml}
\alias{create_schema_xml}
\title{Creates final Mondrian XML schema}
\usage{
  create_schema_xml(engine, table, time_dimension_xml,
    dimension_xml, measure_xml, calculated_member_xml,
    schema_dest, debug = FALSE)
}
\arguments{
  \item{engine}{Data engine. See function
  \code{\link{create_schema}} for details.}

  \item{table}{Table for which schema should be created}

  \item{time_dimension_xml}{Final XML for time dimension}

  \item{dimension_xml}{Final XML for dimensions}

  \item{measure_xml}{Final XML for measures}

  \item{schema_dest}{Schema destination}

  \item{debug}{Print additional information useful for
  debugging.}
}
\description{
  Creates final Mondrian XML schema and writes it to
  specified destination
}

