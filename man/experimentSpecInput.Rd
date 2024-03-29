% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/experimentSpec.R
\name{experimentSpecInput}
\alias{experimentSpecInput}
\title{Module Input for Experiment Specification}
\usage{
experimentSpecInput(
  inputId,
  data,
  mae_name,
  label_experiments = "Select Experiment"
)
}
\arguments{
\item{inputId}{(\code{string})\cr
the ID used to call the module input.}

\item{data}{(\code{reactive})\cr
\verb{reactive(<teal_data>)} holding all the data sets provided
during app initialization after going through the filters.}

\item{mae_name}{(\code{string})\cr
name of the MAE data used in the teal module.}

\item{label_experiments}{(\code{string})\cr label for the experiment selection.}
}
\value{
The UI part.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

This defines the input for the experiment specification.
}
\seealso{
\code{\link[=experimentSpecServer]{experimentSpecServer()}} for the module server and a complete example.
}
