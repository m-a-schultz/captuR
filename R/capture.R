#' #' Capture code block execution and context
#'
#' Evaluates a block of R code and returns an object encapsulating both the
#' resulting environment and the associated metadata for reproducibility. This
#' includes inputs, outputs, code lines, digest values, and optional tags.
#'
#' @param code A block of code surrounded by `{}`.
#' @param save_env Logical; whether to return a named list of created objects.
#' @param tags Optional character vector of user tags.
#' @param digest_algo Digest algorithm to use (default: "sha256").
#' @param verbose Logical; if TRUE, prints a summary of capture.
#' @param include_session Logical; Should session info be included in metadata
#' @return A named list with two components:
#' \describe{
#'   \item{`capture`}{An object of class `capture_info` with detailed metadata about the execution.}
#'   \item{`objects`}{(Optional) A named list of created objects from the code block.}
#' }
#'
#' @seealso [capture_env()], [capture_info()], [get_capture()]
#' @rdname capture_code_block
#' @export
capture_block <- function(code, save_env = TRUE, tags = NULL, digest_algo = "sha256", verbose = FALSE, include_session = FALSE) {
  code_lines <- substitute(code)

  #code_lines <- deparse(code_expr)
  # env <- parent.frame()
  # eval(code_expr, envir = env)
  # dependencies <- extract_dependencies_from_code(code_lines)
  # created <- detect_created_objects(dependencies)
  #
  # result <- build_capture_env(created, code_lines, env, NULL, tags, digest_algo, verbose, include_session)
  # result
  capture_code_lines(code_lines, save_env = save_env, tags = tags, digest_algo = digest_algo,
                     verbose = verbose, include_session = include_session, env=parent.frame())
}

#' @param code_lines An R language object, like that returned by `substitute`
#' @rdname capture_code_block
#' @export
capture_code_lines <- function(code_lines, save_env = TRUE, tags = NULL, digest_algo = "sha256", verbose = FALSE,
                               include_session = FALSE, env=new.env()) {
  expr_block <- as.call(c(quote(`{`), code_lines[1:3]))
  eval(expr_block, envir=env)
  dependencies <- extract_dependencies_from_code(code_lines)
  created <- detect_created_objects(dependencies)

  result <- build_capture_env(created, code_lines, env, NULL, tags, digest_algo, verbose, include_session)
  result
}

#' Source an R script with capture metadata
#'
#' Behaves like base::source() but returns an object summarizing all newly created
#' objects along with detailed capture metadata for reproducibility.
#'
#' @param file Path to the script file.
#' @param local Logical or environment. Passed to `base::source()`. Defaults to parent.frame().
#' @param tags Optional character vector of user-supplied tags.
#' @param digest_algo Digest algorithm for checksums (default: "sha256").
#' @param verbose Print metadata summary? Default FALSE.
#' @param include_session Logical; include session info in metadata.
#' @param ... Additional arguments passed to base::source().
#' @return A `capture_env` object containing all top-level outputs created by
#'   the script, with a shared `capture_info` metadata object accessible via
#'   [get_capture()]. The result is returned invisibly.
#'
#' @seealso [capture_env()], [capture_info()], [get_capture()]
#' @export
capture_source <- function(file, local = parent.frame(), tags = NULL, digest_algo = "sha256",
                           verbose = FALSE, include_session = FALSE, ...) {

  if (!file.exists(file)) stop("File not found: ", file)

  code_lines <- readLines(file, warn = FALSE)
  eval_env <- if (is.environment(local)) local else if (isTRUE(local)) environment() else globalenv()

  base::source(file = file, local = eval_env, ...)

  dependencies <- extract_dependencies_from_code(code_lines)
  outputs <- identify_outputs(dependencies)

  result <- build_capture_env(
    object_names = outputs,
    code_lines = code_lines,
    env = eval_env,
    script_path = file,
    tags = tags,
    digest_algo = digest_algo,
    verbose = verbose,
    include_session = include_session
  )

  invisible(result)
}


#' Save/Load an R object with capture metadata
#'
#' Saves and loads an R object to disk as an RDS file, while attaching detailed metadata
#' about its creation, including the originating code, seed, script path, and
#' dependency information.
#'
#' @return The same R object, with an attached `capture_info` object in the
#'   `"capture"` attribute. This metadata can be accessed using
#'   [get_capture()] and includes information such as object name, timestamp,
#'   script source, code lines, digest, Git commit (if applicable), and
#'   dependency structure.
#'
#' @seealso [capture_info()], [get_capture()], [capture_load()]#' @param object The R object.
#' @param file Destination path.
#' @param script_path Optional script path.
#' @param save_env Save environment variables?
#' @param remove_seed Drop set.seed lines?
#' @param tags Optional tags.
#' @param verbose_guess Verbose script path guessing?
#' @param digest_algo Digest algorithm.
#' @param contextual_code Logical; if TRUE, return all code from set.seed() to object creation.
#' @return The saved object with capture metadata.
#' @rdname capture_save_load
#' @export
capture_save <- function(object, file, script_path = NULL, save_env = FALSE, remove_seed = FALSE,
                         tags = NULL, verbose_guess = FALSE, digest_algo = "sha256", contextual_code = FALSE) {

  obj_name <- deparse(substitute(object))
  env <- parent.frame()

  captured <- capture_object(object, script_path, tags, digest_algo, verbose_guess, contextual_code)

  if (remove_seed && !is.null(attr(captured, "capture")$seed)) {
    attr(captured, "capture")$code_lines <-
      attr(captured, "capture")$code_lines[!grepl("set\\.seed\\s*\\(", attr(captured, "capture")$code_lines)]
  }

  saveRDS(captured, file = file)

  if (save_env && !is.null(attr(captured, "capture")$dependencies)) {
    env_file <- paste0(file, ".env.rds")
    env_vars <- capture_environment_snapshot(attr(captured, "capture")$dependencies, env)
    if (!is.null(env_vars)) saveRDS(env_vars, file = env_file)
  }

  invisible(captured)
}


#' Load a captured R object from file
#' @param file RDS file path.
#' @param check_script Validate script?
#' @return R object with capture attribute.
#' @rdname capture_save_load
#' @export
capture_load <- function(file, check_script = TRUE) {
  tryCatch(
    object <- readRDS(file),
    error = function(e) stop("Error loading object: ", e$message)
  )
  capture <- get_capture(object)
  if (check_script) check_capture(capture)
  object
}

#' Attach metadata to a single object
#'
#' Analyzes the provenance of an R object and attaches metadata to it, without
#' saving it to disk. Metadata includes the code that generated the object, its
#' digest, seed, dependencies, and script/Git information.
#'
#' @param object An R object to capture.
#' @param script_path Optional path to source script.
#' @param tags Optional tags.
#' @param digest_algo Digest algorithm to use.
#' @param verbose_guess Logical; whether to print guess path warnings.
#' @param contextual_code Logical; if TRUE, return all code from set.seed() to object creation.
#' @param include_session Logical; Should session info be included in metadata
#' @return The input object with an attached `"capture"` attribute of class
#'   `capture_info`, which encapsulates all associated metadata for reproducibility
#'   and tracking.
#'
#' @seealso [capture_info()], [get_capture()]
#' @export
capture_object <- function(object, script_path = NULL, tags = NULL, digest_algo = "sha256",
                           verbose_guess = FALSE, contextual_code = TRUE, include_session=FALSE) {
  obj_name <- deparse(substitute(object))
  env <- parent.frame()
  warnings_list <- list()

  if (is.null(script_path)) {
    script_path <- guess_script_path(verbose = verbose_guess)
  }

  code_result <- tryCatch({
    get_generating_code(obj_name = obj_name, script_path = script_path,
                        contextual_code = contextual_code)
  }, error = function(e) {
    warnings_list <<- c(warnings_list, paste("Error getting generating code:", e$message))
    NULL
  })

  code_lines <- if (!is.null(code_result) && !is.null(code_result$code)) unlist(code_result$code) else NULL
  seed_value <- if (!is.null(code_result)) code_result$seed else NULL

  dependencies <- tryCatch({
    get_object_dependencies(obj_name = obj_name, script_path = script_path)
  }, error = function(e) {
    warnings_list <<- c(warnings_list, paste("Error getting dependencies:", e$message))
    NULL
  })

  meta <- compute_capture_metadata(
    object_names = obj_name,
    code_lines = code_lines,
    dependencies = dependencies,
    digest_algo = digest_algo,
    env = env,
    script_path = script_path,
    tags = tags
  )

  capture <- build_capture_info(
    object_name = obj_name,
    timestamp = meta$timestamp,
    script_path = meta$metadata$script_path,
    code_lines = code_lines,
    dependencies = dependencies,
    script_checksum = meta$metadata$script_checksum,
    seed = meta$seed,
    session = if (include_session) sessionInfo() else NULL,
    object_digest = meta$digests,
    git_info = meta$metadata$git_info,
    tags = tags,
    package_versions = meta$package_versions,
    warnings = if (length(warnings_list) > 0) warnings_list else NULL,
    digest_algorithm = digest_algo
  )

  attr(object, "capture") <- capture
  object
}
