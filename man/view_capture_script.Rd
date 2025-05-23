% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/access_funcs.R
\name{view_capture_script}
\alias{view_capture_script}
\title{View Full Script with Metadata from Capture}
\usage{
view_capture_script(object, numbered = TRUE, header = TRUE)
}
\arguments{
\item{object}{An R object with capture metadata.}

\item{numbered}{Logical. Whether to add line numbers. Default TRUE.}

\item{header}{Logical. Whether to include metadata header. Default TRUE.}
}
\description{
Prints the full reconstructed code used to create an object, with metadata headers and line numbers.
}
