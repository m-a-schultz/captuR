#' Compare capture metadata for two objects
#'
#' @param object1 First captured object or capture_info
#' @param object2 Second captured object or capture_info
#' @param ignore Optional character vector of field names to ignore
#' @return An object of class `capture_diff`
#' @export
capture_diff <- function(object1, object2, ignore = NULL) {
  cap1 <- if (inherits(object1, "capture_info")) object1 else get_capture(object1)
  cap2 <- if (inherits(object2, "capture_info")) object2 else get_capture(object2)

  if (!inherits(cap1, "capture_info") || !inherits(cap2, "capture_info")) {
    warning("Both inputs must have capture metadata.")
    return(NULL)
  }

  keys <- unique(c(names(cap1), names(cap2)))
  diff_result <- lapply(keys, function(k) {
    if (k %in% ignore) return(NA)
    v1 <- cap1[[k]]
    v2 <- cap2[[k]]
    if (is.null(v1) && is.null(v2)) return("Identical")
    if (is.null(v1)) return("Only in object2")
    if (is.null(v2)) return("Only in object1")
    if (identical(v1, v2)) return("Identical")
    "Different"
  })
  names(diff_result) <- keys

  structure(
    list(
      object1 = deparse(substitute(object1)),
      object2 = deparse(substitute(object2)),
      result = diff_result,
      ignore = ignore
    ),
    class = "capture_diff"
  )
}

#' Print method for capture_diff
#' @param x A `capture_diff` object.
#' @param ... Ignored.
#' @export
print.capture_diff <- function(x, ...) {
  cat("==== Capture Diff: ", x$object1, " vs ", x$object2, " ====\n", sep = "")
  for (key in names(x$result)) {
    val <- x$result[[key]]
    if (val=='Identical') {
      cat("  - ", key, ": Identical\n")
    } else if (is.list(val)) {
      cat("  - ", key, ": ", val$status, "\n")
      if (!is.null(val$object1)) {
        digest_str1 <- if (is.list(val$object1)) capture_digest_str(val$object1) else deparse(val$object1)
        cat("    Object 1:", digest_str1, "\n")
      }
      if (!is.null(val$object2)) {
        digest_str2 <- if (is.list(val$object2)) capture_digest_str(val$object2) else deparse(val$object2)
        cat("    Object 2:", digest_str2, "\n")
      }
    } else if (is.na(val)) {
      cat("  - ", key, ": Ignored\n")
    }
  }
  cat("=========================================\n")
}
capture_digest_str <- function(digest_list, max = 3) {
  keys <- names(digest_list)
  n <- length(keys)
  shown <- head(keys, max)
  text <- paste0(shown, ":", substr(unlist(digest_list[shown]), 1, 8), collapse = ", ")
  if (n > max) paste0(text, ", ... [", n, " total]") else text
}


#' Summary method for capture_diff
#' @param object A `capture_diff` object.
#' @param ... Ignored.
#' @export
summary.capture_diff <- function(object, ...) {
  result <- object$result
  list(
    identical_fields = names(result)[sapply(result, function(v) v=='Identical')],
    differing_fields = names(result)[sapply(result, function(v) v!='Identical')],
    ignored_fields = names(result)[sapply(result, function(x) is.na(x))]
  )
}


#' Convert capture_diff to list
#' @param x A `capture_diff` object.
#' @param ... Ignored.
#' @return A named list of field comparisons.
#' @export
as.list.capture_diff <- function(x, ...) {
  x$result
}

#' Compare capture fields (logical check)
#'
#' Performs a field-by-field logical comparison of two capture metadata objects.
#'
#' @param object1 First captured object or `capture_info`.
#' @param object2 Second captured object or `capture_info`.
#' @param ignore Character vector of fields to ignore (optional).
#' @return Named list indicating which fields are identical (`TRUE`), different (`FALSE`), or ignored (`NA`).
#' @export
compare_capture <- function(object1, object2, ignore = NULL) {
  cap1 <- get_capture(object1)
  cap2 <- get_capture(object2)

  if (!inherits(cap1, "capture_info") || !inherits(cap2, "capture_info")) {
    warning("Objects must both have capture metadata.")
    return(NULL)
  }

  keys <- unique(c(names(cap1), names(cap2)))
  compare_fields(cap1, cap2, keys, ignore)
}

#' Compare two values with optional ignore logic
#' @keywords internal
compare_fields <- function(x, y, keys, ignore = NULL) {
  setNames(lapply(keys, function(k) {
    if (k %in% ignore) return(NA)
    v1 <- x[[k]]
    v2 <- y[[k]]
    identical(v1, v2)
  }), keys)
}

#' Compare original and rerun outputs for a list of objects
#' @keywords internal
compare_outputs <- function(outputs, original, rerun) {
  setNames(sapply(outputs, function(name) {
    if (!name %in% names(rerun)) return(FALSE)
    orig <- if (inherits(original, "capture_env")) original[[name]] else original
    new <- rerun[[name]]
    identical(remove_capture(orig), remove_capture(new))
  }), outputs)
}

#' Re-run Captured Code and Validate Outputs
#'
#' Re-evaluates the original code used to create a captured object and compares
#' the resulting outputs to the original objects. Useful for verifying that
#' captured code remains reproducible and consistent.
#'
#' @param obj A captured object or `capture_env` with capture metadata.
#' @param overall Logical; if `TRUE`, returns a single `TRUE`/`FALSE` indicating
#'   whether all outputs matched. If `FALSE` (default), returns a named logical
#'   vector indicating which outputs matched.
#'
#' @return A logical vector (named by object name) or a single logical value if `overall = TRUE`.
#' @export
#'
#' @examples
#' \dontrun{
#' result <- check_captured_code(my_model)
#' if (any(!result)) warning("Some outputs did not match.")
#'
#' # Check if everything matches in a single logical
#' check_captured_code(my_model, overall = TRUE)
#' }
check_captured_code <- function(obj, overall = FALSE) {
  cap <- get_capture(obj)
  if (is.null(cap) || is.null(cap$code_lines)) stop("No valid capture metadata found.")

  newcap <- tryCatch(capture_code_lines(cap$code_lines), error = function(e) NULL)

  outputs <- if (inherits(obj, "capture_env")) names(obj) else cap$outputs %||% cap$object_name

  if (is.null(newcap)) {
    warning("Error during code evaluation.")
    return(setNames(rep(FALSE, length(outputs)), outputs))
  }

  result <- compare_outputs(outputs, obj, newcap)
  if (overall) all(result) else result
}



