% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/capture.R
\name{capture_block}
\alias{capture_block}
\alias{capture_code_lines}
\title{#' Capture code block execution and context}
\usage{
capture_block(
  code,
  save_env = TRUE,
  tags = NULL,
  digest_algo = "sha256",
  verbose = FALSE,
  include_session = FALSE
)

capture_code_lines(
  code_lines,
  save_env = TRUE,
  tags = NULL,
  digest_algo = "sha256",
  verbose = FALSE,
  include_session = FALSE,
  env = new.env()
)
}
\arguments{
\item{code}{A block of code surrounded by `{}`.}

\item{save_env}{Logical; whether to return a named list of created objects.}

\item{tags}{Optional character vector of user tags.}

\item{digest_algo}{Digest algorithm to use (default: "sha256").}

\item{verbose}{Logical; if TRUE, prints a summary of capture.}

\item{include_session}{Logical; Should session info be included in metadata}

\item{code_lines}{An R language object, like that returned by `substitute`}
}
\value{
A named list with two components:
\describe{
  \item{`capture`}{An object of class `capture_info` with detailed metadata about the execution.}
  \item{`objects`}{(Optional) A named list of created objects from the code block.}
}
}
\description{
Evaluates a block of R code and returns an object encapsulating both the
resulting environment and the associated metadata for reproducibility. This
includes inputs, outputs, code lines, digest values, and optional tags.
}
\seealso{
[capture_env()], [capture_info()], [get_capture()]
}
