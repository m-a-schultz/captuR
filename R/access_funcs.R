
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Check if an object has capture metadata
#'
#' Determines whether the object has a valid capture metadata attribute.
#'
#' @param object An R object.
#' @return TRUE if the object has a capture attribute of class `capture_info`, FALSE otherwise.
#' @export
has_capture <- function(object) {
  inherits(attr(object, "capture"), "capture_info")
}

#' Access capture metadata from an R object
#'
#' Retrieves the metadata stored during object capture.
#' @param object An R object saved using `capture_save()`.
#' @return A `capture_info` object or NULL if unavailable.
#' @export
get_capture <- function(object) {
  attr(object, "capture")
}

#' View the generating code from capture metadata
#'
#' @param object A captured object.
#' @export
view_capture_code <- function(object) {
  capture <- get_capture(object)
  if (!is.null(capture) && !is.null(capture$code_lines)) {
    cat("Generating Code:\n")
    cat(paste(unlist(capture$code_lines), collapse = "\n"), "\n")
  } else {
    cat("Generating code not available in capture metadata.\n")
  }
}

#' List input dependencies from capture metadata
#'
#' @param object A captured object.
#' @return A list of dependencies or NULL.
#' @export
list_capture_dependencies <- function(object) {
  capture <- get_capture(object)
  capture$dependencies
}

#' Retrieve the capture timestamp
#'
#' @param object A captured object.
#' @return POSIXct timestamp or NULL.
#' @export
capture_timestamp <- function(object) {
  capture <- get_capture(object)
  capture$timestamp
}


#' Get the script path from capture metadata
#'
#' @param object A `capture_info` object.
#' @return Character string of the script path.
#' @export
capture_script_path <- function(object) UseMethod("capture_script_path")

#' @method capture_script_path capture_info
#' @export
capture_script_path.capture_info <- function(object) {
  object$script_path
}

#' Get tags from capture metadata
#'
#' @param object An R object with capture metadata.
#' @return Character vector of tags or NULL if not available.
#' @export
capture_tags <- function(object) {
  capture <- attr(object, "capture")
  if (!is.null(capture)) capture$tags else NULL
}

#' Get digest from capture metadata
#'
#' @param object An R object with capture metadata.
#' @return Digest string or NULL if not available.
#' @export
capture_digest <- function(object) {
  capture <- attr(object, "capture")
  if (!is.null(capture)) capture$object_digest else NULL
}

#' Capture Metadata Summary for Multiple Objects
#'
#' Returns a data frame summarizing key metadata from a list of captured objects.
#'
#' @param objects A named list of R objects (or captured environment).
#' @return A data.frame with metadata summary.
#' @export
capture_summary <- function(objects) {
  if (inherits(objects, "capture_env")) {
    cap <- attr(objects, "capture")
    object_names <- names(objects)
  } else {
    cap <- NULL
    object_names <- names(objects)
  }


  if (!is.list(objects)) stop("Input must be a list or capture_env.")

  summaries <- lapply(names(objects), function(name) {
    obj <- objects[[name]]
    cap <- attr(obj, "capture")
    if (is.null(cap)) return(NULL)

    data.frame(
      object = name,
      script = cap$script_path,
      timestamp = as.character(cap$timestamp),
      digest = as.character(cap$object_digest[[name]] %||% cap$object_digest),
      git_commit = cap$git_info$commit %||% NA_character_,
      tags = paste(cap$tags, collapse = ", "),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, summaries)
}

#' View Full Script with Metadata from Capture
#'
#' Prints the full reconstructed code used to create an object, with metadata headers and line numbers.
#'
#' @param object An R object with capture metadata.
#' @param numbered Logical. Whether to add line numbers. Default TRUE.
#' @param header Logical. Whether to include metadata header. Default TRUE.
#' @export
view_capture_script <- function(object, numbered = TRUE, header = TRUE) {
  capture <- attr(object, "capture")
  if (is.null(capture) || is.null(capture$code_lines)) {
    cat("No capture code available.\n")
    return(invisible(NULL))
  }

  code <- as.character(capture$code_lines)

  if (header) {
    meta <- c(
      "===== Reconstructed Capture Script =====",
      paste("Object:", capture$object_name),
      paste("Timestamp:", format(capture$timestamp)),
      paste("Digest (", capture$digest_algorithm, "): ",
            capture$object_digest[[capture$object_name]] %||% capture$object_digest, sep = ""),
      if (!is.null(capture$git_info)) paste("Git Commit:", capture$git_info$commit) else NULL,
      if (!is.null(capture$tags)) paste("Tags:", paste(capture$tags, collapse = ", ")) else NULL,
      "----------------------------------------"
    )
    cat(paste(meta, collapse = "\n"), "\n\n")
  }

  if (numbered) {
    numbered_lines <- sprintf("%4d: %s", seq_along(code), code)
    cat(paste(numbered_lines, collapse = "\n"), "\n")
  } else {
    cat(paste(code, collapse = "\n"), "\n")
  }

  invisible(NULL)
}

#' @method get_capture capture_env
#' @export
get_capture.capture_env <- function(object) {
  attr(object, "capture")
}
