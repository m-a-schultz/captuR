% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class.R
\name{capture_env}
\alias{capture_env}
\title{Create a captured environment object}
\usage{
capture_env(objects, capture)
}
\arguments{
\item{objects}{A named list of R objects.}

\item{capture}{A `capture_info` object with metadata.}
}
\value{
A `capture_env` object that behaves like a list with attached metadata.
}
\description{
Constructs a special list-like object that holds a group of captured R objects
along with a shared `capture_info` metadata structure. This is typically
returned by top-level capture functions that track multiple outputs.
}
\details{
The `capture_env` behaves like a named list, and individual objects may be
accessed using `$` or `[[` operators. Each object accessed in this way will
have the shared `"capture"` metadata attached automatically.
}
\section{Structure}{

A `capture_env` is a list of named objects with an attached attribute:
\describe{
  \item{[objects]}{Each named object created during a script or code block execution.}
  \item{`attr(., "capture")`}{A `capture_info` object describing shared metadata for all objects.}
}
}

\section{Access}{

\itemize{
  \item Use `x$name` or `x[[name]]` to access individual objects.
  \item The shared capture metadata is propagated to each extracted object.
  \item Use [get_capture()] to retrieve metadata from either the `capture_env` or its elements.
}
}

\seealso{
[capture_info()], [capture_block()], [capture_source()], [get_capture()]
}
