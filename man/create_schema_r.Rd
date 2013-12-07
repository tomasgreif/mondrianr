\name{create_schema_r}
\alias{create_schema_r}
\title{Create OLAP cube definition based on R data frame}
\usage{
  create_schema_r(data, primary_key, schema_dest,
    data_source_dest, numeric_aggregators = NA,
    date_aggregators = NA, max_time = "2015-01-01",
    min_time = "2001-01-01",
    dimension_data_types = c("character", "integer"),
    numeric_data_types = c("numeric", "integer", "double"))
}
\arguments{
  \item{data}{Name of R data frame. Passed as object, not
  string (correct: \code{data_frame}, wrong:
  \code{'data_frame'}..}

  \item{primary_key}{Name of column with primary key in
  table specified in \code{table}. Primary key is not
  strictly required, however it is necesarry that values in
  specified column are unique.}

  \item{schema_dest}{Where XML file with cube definition
  should be written. Has to include file name. Defaults to
  current working directory}

  \item{data_source_dest}{Where data source definition
  should be written. Has to include file name. This
  definition is Saiku specific}

  \item{numeric_aggregators}{Vector defining what Mondrian
  aggregators will be used for measures. Can be any
  combination of
  \code{'sum','count','min','max','avg','distinct-count'}.
  Defaults to all aggregators}

  \item{date_aggregators}{Vector defining what Mondrian
  aggregators will be used for date columns. Can be any
  combination of
  \code{'sum','count','min','max','avg','distinct-count'}.
  Defaults to \code{'count'}.}

  \item{dimension_data_types}{Vector definining what data
  types will be used to create dimensions. Defaults to
  \code{'character','integer'}. Date columns will be
  included automatically even if not specified.}

  \item{numeric_data_types}{Vector defining what data types
  will be in measures. Defaults to
  \code{'numeric','integer','double'}. Date columns will be
  included automatically.}

  \item{min_time}{Start of time dimension. Time dimension
  is generated dynamically during function execution.
  Defaults to 2001-01-01.}

  \item{max_time}{End of time dimension. Defaults to
  2015-01-01.}
}
\description{
  Create OLAP cube definition based on R data frame
}

