% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/capture_helpers.R
\name{code_snippet}
\alias{code_snippet}
\title{Generate a compact code snippet from capture code lines}
\usage{
code_snippet(code_lines, max_chars = 80)
}
\arguments{
\item{code_lines}{Character vector of R code lines (e.g., from capture metadata).}

\item{max_chars}{Maximum total character width (default: 80).}
}
\value{
A single character string with the snippet.
}
\description{
Returns a condensed, one-line summary of the first few meaningful lines of code.
}
\keyword{internal}
