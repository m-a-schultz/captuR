% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/capture_helpers.R
\name{is_stochastic_code}
\alias{is_stochastic_code}
\title{Detect Stochastic Behavior in Code Lines}
\usage{
is_stochastic_code(code_lines, funcs = stochastic_functions())
}
\arguments{
\item{code_lines}{Character vector of R code lines.}

\item{funcs}{Optional character vector of stochastic function names to match.}
}
\value{
Logical indicating whether code is stochastic.
}
\description{
Determines whether any known stochastic functions appear in the code.
}
