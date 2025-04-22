#' Create a capture_info object
#'
#' Constructs a structured metadata object describing the full context in which
#' an R object or environment was captured. This includes information about the
#' object’s name, digest, timestamp, script source, code history, dependencies,
#' session info, Git metadata, seed state, and other provenance details.
#'
#' The `capture_info` object is attached as an attribute to captured R objects
#' or environments under the name `"capture"` and is used for auditing,
#' reproducibility, validation, and code regeneration.
#'
#' @section Structure:
#' A `capture_info` object is a named list with the following components:
#' \describe{
#'   \item{inputs}{Character vector of objects that were inputs (read from) during generation.}
#'   \item{outputs}{Character vector of objects created or updated.}
#'   \item{capture_version}{Character string indicating captuR version.}
#'   \item{object_name}{The name of the primary object (or comma-separated names for multiple).}
#'   \item{timestamp}{POSIXct timestamp indicating when the capture was performed.}
#'   \item{script_path}{Path to the R script that generated the object (if available).}
#'   \item{code_lines}{Character vector of R code used to generate the object(s).}
#'   \item{dependencies}{List of `ScriptNodeInfo` objects from `CodeDepends` that describe code structure.}
#'   \item{script_checksum}{Digest hash of the script file at capture time.}
#'   \item{seed}{The nearest `set.seed()` call found before object creation, if present.}
#'   \item{session}{Optional `sessionInfo()` result if session info was captured.}
#'   \item{object_digest}{Named list of digest hashes for each object, or a single string for one object.}
#'   \item{git_info}{Named list with Git commit, author, message, and date (if available).}
#'   \item{tags}{Optional character vector of user-supplied tags.}
#'   \item{package_versions}{Named character vector of package versions used in code.}
#'   \item{warnings}{List of warning messages generated during capture (if any).}
#'   \item{digest_algorithm}{Digest algorithm used (e.g., `"sha256"`).}
#' }
#'
#' @return An object of class `capture_info`, suitable for attaching as metadata.
#' @seealso [get_capture()], [capture_env()], [capture_save()], [capture_block()]
#' @export
#' @param version captuR version string.
#' @param object_name Name of the object being saved.
#' @param timestamp Time when the object was saved.
#' @param script_path Path to the script that generated the object.
#' @param code_lines Character vector of code lines used to generate the object.
#' @param dependencies A list of `ScriptNodeInfo` objects (from CodeDepends).
#' @param script_checksum Digest checksum of the script file.
#' @param seed Any `set.seed()` calls found in the generating code.
#' @param is_stochastic Logical indicating whether stochastic code was detected
#' @param session The output of `sessionInfo()` at save time.
#' @param object_digest Digest hash of the object itself.
#' @param git_info Git metadata as a named list (optional).
#' @param tags Character vector of user-supplied tags (optional).
#' @param package_versions Named character vector of package versions used in dependencies.
#' @param warnings List of warning messages generated during capture.
#' @param digest_algorithm The digest algorithm used (e.g., "sha256").
#' @return An object of class `capture_info`.
#' @export
capture_info <- function(inputs, outputs, version, object_name, timestamp, script_path, code_lines, dependencies,
                         script_checksum, seed, is_stochastic, session, object_digest, git_info = NULL, tags = NULL,
                         package_versions = NULL, warnings = NULL, digest_algorithm = "sha256") {
  structure(
    list(
      inputs = inputs,
      outputs = outputs,
      capture_version = version,
      object_name = object_name,
      timestamp = timestamp,
      script_path = script_path,
      code_lines = code_lines,
      dependencies = dependencies,
      script_checksum = script_checksum,
      seed = seed,
      is_stochastic = is_stochastic,
      session = session,
      object_digest = object_digest,
      git_info = git_info,
      tags = tags,
      package_versions = package_versions,
      warnings = warnings,
      digest_algorithm = digest_algorithm
    ),
    class = "capture_info"
  )
}

#' Summary method for capture metadata
#'
#' @param object A `capture_info` object.
#' @param ... Ignored.
#' @method summary capture_info
#' @return A named list summarizing key metadata.
#' @export
summary.capture_info <- function(object, ...) {
  size_kb <- round(object.size(object) / 1024, 2)

  list(
    object_name = object$object_name,
    timestamp = object$timestamp,
    inputs = object$inputs,
    outputs = object$outputs,
    seed = object$seed,
    tags = object$tags,
    script_path = object$script_path,
    git_commit = object$git_info$commit %||% NULL,
    code_snippet = code_snippet(object$code_lines),
    script_checksum = object$script_checksum,
    digest_algorithm = object$digest_algorithm,
    version = object$capture_version,
    metadata_size_kb = size_kb
  )
}

#' Print summary of capture metadata
#'
#' @param x A `capture_info` object.
#' @param ... Ignored.
#' @method print capture_info
#' @export
print.capture_info <- function(x, ...) {
  s <- summary(x)

  cat("==== Capture Metadata Summary ====\n")

  cat("Inputs:          ", if (length(s$inputs) == 0) "None" else paste(s$inputs, collapse = ", "), "\n")
  cat("Outputs:         ", if (length(s$outputs) == 0) "None" else paste(s$outputs, collapse = ", "), "\n")
  cat("Seed:            ", if (!is.null(s$seed)) s$seed else "None detected", "\n")
  cat("Timestamp:       ", format(s$timestamp), "\n")

  if (!is.null(s$tags)) {
    cat("Tags:            ", paste(s$tags, collapse = ", "), "\n")
  }

  cat("Script Path:     ", s$script_path %||% "(not recorded)", "\n")

  if (!is.null(s$git_commit)) {
    cat("Git Commit:      ", s$git_commit, "\n")
  }

  cat("Code Snippet:    ", s$code_snippet, "\n")

  if (!is.null(s$script_checksum)) {
    cat("Script Checksum (", s$digest_algorithm, "): ", s$script_checksum, "\n", sep = "")
  } else {
    cat("Script Checksum: (not available)\n")
  }

  cat("Metadata Size:   ", s$metadata_size_kb, "KB\n")
  cat("Version:         ", s$version, "\n")
  cat("==================================\n")
}



#' Convert a capture_info object to a list
#'
#' @param x A `capture_info` object.
#' @param ... Ignored.
#' @return Named list of metadata.
#' @method as.list capture_info
#' @export
as.list.capture_info <- function(x, ...) {
  unclass(x)
}

#' Create a captured environment object
#'
#' Constructs a special list-like object that holds a group of captured R objects
#' along with a shared `capture_info` metadata structure. This is typically
#' returned by top-level capture functions that track multiple outputs.
#'
#' The `capture_env` behaves like a named list, and individual objects may be
#' accessed using `$` or `[[` operators. Each object accessed in this way will
#' have the shared `"capture"` metadata attached automatically.
#'
#' @section Structure:
#' A `capture_env` is a list of named objects with an attached attribute:
#' \describe{
#'   \item{[objects]}{Each named object created during a script or code block execution.}
#'   \item{`attr(., "capture")`}{A `capture_info` object describing shared metadata for all objects.}
#' }
#'
#' @section Access:
#' \itemize{
#'   \item Use `x$name` or `x[[name]]` to access individual objects.
#'   \item The shared capture metadata is propagated to each extracted object.
#'   \item Use [get_capture()] to retrieve metadata from either the `capture_env` or its elements.
#' }
#'
#' @param objects A named list of R objects.
#' @param capture A `capture_info` object with metadata.
#' @return A `capture_env` object that behaves like a list with attached metadata.
#' @seealso [capture_info()], [capture_block()], [capture_source()], [get_capture()]
#' @export
capture_env <- function(objects, capture) {
  structure(
    objects,
    capture = capture,
    class = "capture_env"
  )
}


#' Print method for capture_env
#'
#' Displays a summary of the captured environment.
#'
#' @param x A `capture_env` object.
#' @param ... Ignored.
#' @return The object (invisible).
#' @export
print.capture_env <- function(x, ...) {
  cat("Captured environment with", length(x), "objects:\n")
  cat("Metadata for:", attr(x, "capture")$object_name, "\n")
  invisible(x)
}

#' Convert capture_env to list
#'
#' @param x A `capture_env` object.
#' @param ... Ignored.
#' @return Named list of contained objects.
#' @export
as.list.capture_env <- function(x, ...) {
  unclass(x)  # removes class but keeps attributes
}


#' Extract an object from capture_env
#'
#' Allows access to objects via `$` operator.
#'
#' @param x A `capture_env` object.
#' @param name Name of object to extract.
#' @return The extracted object.
`$.capture_env` <- function(x, name) {
  obj <- x[[name]]
  attr(obj, "capture") <- attr(x, "capture")
  obj
}

#' Extract an object from capture_env
#'
#' Allows access to objects via `$` operator.
#'
#' @param x A `capture_env` object.
#' @param i Index of object to extract.
#' @return The extracted object.
`[[.capture_env` <- function(x, i) {
  obj <- NextMethod("[[")
  attr(obj, "capture") <- attr(x, "capture")
  obj
}


remove_capture <- function(x){
  attr(x, 'capture') <- NULL
  x
}
