


#' Compute digests for a list of object names
#'
#' @param object_names Character vector of object names
#' @param env Environment to look up objects in
#' @param algo Digest algorithm to use (default: "sha256")
#' @return Named list of object digests
#' @keywords internal
compute_object_digests <- function(object_names, env = parent.frame(), algo = "sha256") {
  requireNamespace("digest", quietly = TRUE)
  digests <- lapply(object_names, function(obj) digest::digest(get(obj, envir = env), algo = algo))
  names(digests) <- object_names
  digests
}

#' Get metadata from a script path
#'
#' @param path File path to the R script
#' @param algo Digest algorithm to use
#' @return Named list with script metadata
#' @keywords internal
get_script_metadata <- function(path, algo = "sha256") {
  list(
    script_path = normalizePath(path, mustWork = FALSE),
    script_checksum = if (file.exists(path)) digest::digest(file = path, algo = algo) else NULL,
    timestamp = Sys.time(),
    git_info = tryCatch(get_git_info(path), error = function(e) NULL)
  )
}

#' Extract set.seed() call from code lines
#'
#' @param code_lines Character vector of R code lines
#' @return Last set.seed() line found, or NULL
#' @keywords internal
get_code_seed <- function(code_lines) {
  seeds <- grep("set\\.seed\\s*\\(", code_lines, value = TRUE)
  if (length(seeds) > 0) tail(seeds, 1) else NULL
}

#' Get package versions from CodeDepends dependencies
#'
#' Extracts used packages based on namespaced functions in CodeDepends analysis.
#'
#' @param dependencies List of ScriptNodeInfo objects
#' @return Named character vector of package versions
get_package_versions <- function(dependencies) {
  if (length(dependencies) == 0) return(character())

  # Only include functions that were actually used (TRUE)
  all_functions <- unlist(lapply(dependencies, function(x) names(x@functions)[x@functions]))

  # Extract package names from pkg::fun format
  pkg_calls <- all_functions[grepl("::", all_functions)]
  pkgs <- unique(sub("::.*", "", pkg_calls))
  pkgs <- pkgs[nzchar(pkgs)]

  stats::setNames(
    sapply(pkgs, function(pkg) {
      tryCatch(as.character(utils::packageVersion(pkg)), error = function(e) NA_character_)
    }),
    pkgs
  )
}


#' Snapshot objects from environment
#'
#' @param object_names Character vector of object names
#' @param env Environment to read from
#' @return Named list of object values
#' @keywords internal
capture_environment_snapshot <- function(object_names, env = parent.frame()) {
  mget(object_names, envir = env)#, ifnotfound = list())
}


#' Check if capture script checksum is valid
#'
#' Validates that the current script content matches the recorded checksum in the capture metadata.
#'
#' @param capture A `capture_info` object.
#' @return Logical, TRUE if checksum matches current script.
#' @export
is_script_checksum_valid <- function(capture) {
  if (inherits(capture, "capture_info") &&
      !is.null(capture$script_path) &&
      file.exists(capture$script_path) &&
      !is.null(capture$script_checksum)) {

    current_checksum <- tryCatch(
      digest::digest(file = capture$script_path, algo = capture$digest_algorithm),
      error = function(e) NULL
    )

    return(identical(current_checksum, capture$script_checksum))
  }
  FALSE
}


#' Return summary list from capture metadata
#'
#' Provides a simplified summary of key capture metadata.
#'
#' @param object An object with a `capture` attribute.
#' @return Named list with name, timestamp, digest, script path, Git commit, and tags.
#' @export
capture_summary_list <- function(object) {
  capture <- attr(object, "capture")
  if (is.null(capture)) return(NULL)
  list(
    name = capture$object_name,
    timestamp = capture$timestamp,
    digest = capture$object_digest,
    script = capture$script_path,
    git = capture$git_info$commit,
    tags = capture$tags
  )
}

#' Pipe-friendly capture-save operator
#'
#' A forward-pipeable operator to save an object with capture metadata.
#'
#' @param object The object to be saved.
#' @param file File path to save the object.
#' @return The saved object (invisible).
#' @export
`%capture%` <- function(object, file) {
  capture_save(object, file)
}

#' Generate a compact code snippet from capture code lines
#'
#' Returns a condensed, one-line summary of the first few meaningful lines of code.
#'
#' @param code_lines Character vector of R code lines (e.g., from capture metadata).
#' @param max_chars Maximum total character width (default: 80).
#' @return A single character string with the snippet.
#' @keywords internal
code_snippet <- function(code_lines, max_chars = 80) {
  if (is.null(code_lines) || length(code_lines) == 0) return("(not available)")

  # Clean code: trim whitespace and strip comments
  lines <- gsub("#.*", "", trimws(code_lines))
  lines <- lines[nzchar(lines)]  # drop empty lines

  # Build preview until max_chars is reached
  preview <- character()
  total_length <- 0
  for (line in lines) {
    added_length <- nchar(line) + if (length(preview) > 0) 2 else 0  # +2 for "; "
    if ((total_length + added_length) > max_chars) break
    preview <- c(preview, line)
    total_length <- total_length + added_length
  }

  snippet <- paste(preview, collapse = "; ")
  if (length(preview) < length(lines)) snippet <- paste0(snippet, "; ...")
  snippet
}

#' List Known Stochastic Function Names
#'
#' Returns a list of commonly used R functions that generate randomness.
#'
#' @return A character vector of function names (e.g., "rnorm", "sample").
#' @export
stochastic_functions <- function() {
  c(
    # Base R
    "sample", "rnorm", "runif", "rbinom", "rexp", "rpois", "rchisq", "rt", "rf",
    "rgeom", "rhyper", "rlogis", "rnbinom", "rbeta", "rgamma", "rweibull", "rlnorm",
    "rcauchy", "rwilcox", "rsignrank",

    # MASS
    "mvrnorm",

    # truncnorm
    "rtruncnorm",

    # extraDistr (some examples)
    "rpareto", "rgenpois", "rlaplace", "rlind", "rnorminvgauss", "rzipf", "rbetapr",

    # EnvStats
    "rpareto", "rlaplace",

    # VGAM
    "ralap", "rbilogis", "rposbinom",

    # actuar
    "rinvweibull", "rgenbeta", "rztpois",

    # dqrng
    "dqsample", "dqrunif", "dqrnorm",

    # Other potential additions
    "rtriangle", "rtriang", "rtriangular", "rhalfnorm", "rbernoulli"
  )

}
#' Detect Stochastic Behavior in Code Lines
#'
#' Determines whether any known stochastic functions appear in the code.
#'
#' @param code_lines Character vector of R code lines.
#' @param funcs Optional character vector of stochastic function names to match.
#' @return Logical indicating whether code is stochastic.
#' @export
is_stochastic_code <- function(code_lines, funcs = stochastic_functions()) {
  pattern <- paste0("\\b(", paste(funcs, collapse = "|"), ")\\b")
  any(grepl(pattern, code_lines))
}
