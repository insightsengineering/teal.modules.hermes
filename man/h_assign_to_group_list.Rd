% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sampleVarSpec.R
\name{h_assign_to_group_list}
\alias{h_assign_to_group_list}
\title{Helper Function For Group List Creation}
\usage{
h_assign_to_group_list(x)
}
\arguments{
\item{x}{(named \code{list} of \code{character})\cr input assignment list.}
}
\value{
A combination list.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

This helper function takes an assignment list and converts it to a
group list.
}
\examples{
assign_list <- list(
  "ASIAN" = "1",
  "BLACK OR AFRICAN AMERICAN" = "1",
  "MULTIPLE" = "2",
  "UNKNOWN" = "2",
  "WHITE" = "4"
)
objective_list <- list(
  "ASIAN/BLACK OR AFRICAN AMERICAN" = c("ASIAN", "BLACK OR AFRICAN AMERICAN"),
  "MULTIPLE/UNKNOWN" = c("MULTIPLE", "UNKNOWN"),
  "WHITE" = "WHITE"
)
result_list <- h_assign_to_group_list(assign_list)
stopifnot(identical(result_list, objective_list))
}
