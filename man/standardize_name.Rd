\name{standardize_name}
\alias{standardize_name}
\title{Standardize name}
\usage{
standardize_name(name = NA, debug = FALSE)
}
\arguments{
  \item{name}{String to be standardized.}

  \item{debug}{Print additional information useful for
  debugging.}
}
\description{
Clean name into more human-readable form. Removes
undescores, makes first letter upper-case.
}
\examples{
standardize_name("column_name")
}

