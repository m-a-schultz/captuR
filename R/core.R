#' @title R Object Capture and Context Tracking
#' @description Save R objects with associated context: code, dependencies, digests, and Git/script metadata.
#' @name captuR
#' @keywords internal
"_PACKAGE"

#' @keywords internal
CAPTUR_VERSION <- "1.0"
#' @importFrom utils packageVersion sessionInfo tail
#' @importFrom rstudioapi getSourceEditorContext isAvailable

NULL

#' Get the current captuR version.
#' @export
capture_version <- function() CAPTUR_VERSION

#' Extract Generating Code and Seed Call
#'
#' Identifies the code lines that generated an object using `CodeDepends`, and locates
#' the closest preceding `set.seed()` call if available.
#'
#' @param expr The object whose capture is being traced.
#' @param script_path Optional path to the R script file. Guessed if NULL.
#' @param obj_name Optional character name of the object. Defaults to `deparse(substitute(expr))`.
#' @param dependencies Optional precomputed dependency list from `get_object_dependencies()`.
#' @param contextual_code Logical; if TRUE, return all code from set.seed() to object creation.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{code}{List of code lines related to object creation.}
#'   \item{seed}{Optional set.seed line found before the last code line.}
#' }
#' @export
get_generating_code <- function(expr, script_path = NULL, obj_name = NULL, contextual_code = TRUE) {
  if (is.null(obj_name))
    obj_name <- deparse(substitute(expr))

  if (is.null(script_path) || !file.exists(script_path)) return(NULL)

  script <- tryCatch(CodeDepends::readScript(script_path), error = function(e) return(NULL))
  if (is.null(script)) return(NULL)

  info <- CodeDepends::getInputs(script)
  deps_idx <- tryCatch(CodeDepends::getDependsThread(obj_name, info), error = function(e) return(NULL))
  if (length(deps_idx) == 0) return(NULL)

  # Try to identify the last set.seed() call *before* the last dependency
  seed_line <- NULL
  seed_idx <- NA
  if (contextual_code) {
    block_texts <- sapply(info[seq_len(max(deps_idx))], function(x) paste(trimws(deparse(x@code)), collapse = "\n"))
    matches <- grep("set\\.seed\\s*\\(", block_texts)
    if (length(matches) > 0) {
      seed_idx <- max(matches)
      seed_line <- block_texts[seed_idx]
    }
  }

  # Choose the block range
  selected_blocks <- if (contextual_code && is.finite(seed_idx)) {
    info[seed_idx:max(deps_idx)]
  } else {
    info[deps_idx]
  }

  code_lines <- unlist(lapply(selected_blocks, function(x) x@code))

  list(code = code_lines, seed = seed_line)
}

# get_generating_code <- function(expr, script_path = NULL, obj_name = NULL, dependencies = NULL, contextual_code = FALSE) {
#   if (is.null(obj_name))
#     obj_name <- deparse(substitute(expr))
#
#   obj_deps <- if (!is.null(dependencies)){
#     dependencies
#   }else{
#     get_object_dependencies(obj_name, script_path)
#   }
#   if (length(obj_deps) == 0) return(NULL)
#
#   code_lines <- NULL
#   seed_line <- NULL
#   seed_line_num <- NA
#
#   #last_line <- suppressWarnings(max(unlist(lapply(obj_deps, function(x) x@lineNum)), na.rm = TRUE))
#
#   if (!is.null(script_path) && file.exists(script_path)){# && is.finite(last_line)) {
#     script_lines <- tryCatch(readLines(script_path, warn = FALSE), error = function(e) character())
#
#     if (length(script_lines) > 0 && last_line > 1) {
#       seed_matches <- grep("set\\.seed\\s*\\(", script_lines[1:last_line], perl = TRUE)
#       if (length(seed_matches) > 0) {
#         seed_line_num <- max(seed_matches)
#         seed_line <- script_lines[seed_line_num]
#       }
#
#       if (contextual_code && is.finite(seed_line_num)) {
#         code_lines <- script_lines[seed_line_num:last_line]
#       } else {
#         code_lines <- tryCatch(
#           unlist(lapply(obj_deps, function(x) x@code)),
#           error = function(e) {
#             warning("Error extracting generating code via CodeDepends: ", e$message)
#             return(NULL)
#           }
#         )
#       }
#     }
#   }
#
#   list(code = code_lines, seed = seed_line)
# }

#' @keywords internal
is_initialized <- function(obj_name, obj_deps) {
  if (length(obj_deps) == 0) return(FALSE)
  outs <- lapply(obj_deps, function(x) x@outputs)
  ins <- lapply(obj_deps, function(x) x@inputs)
  first_out <- min(which(sapply(outs, function(x) obj_name %in% x)), Inf)
  first_in <- min(which(sapply(ins, function(x) obj_name %in% x)), Inf)
  first_out < first_in
}

#' @keywords internal
initialized_objs <- function(obj_deps) {
  if (length(obj_deps) == 0) return(c())
  outs <- lapply(obj_deps, function(x) x@outputs)
  ins <- lapply(obj_deps, function(x) x@inputs)
  objects <- sort(unique(c(unlist(outs),unlist(ins))))
  objects[sapply(objects, function(obj_name){
    first_out <- min(which(sapply(outs, function(x) obj_name %in% x)), Inf)
    first_in <- min(which(sapply(ins, function(x) obj_name %in% x)), Inf)
    first_out < first_in
  })]
}


# Possibly deprecated?
#' @keywords internal
list_object_dependencies <- function(expr, script_path, obj_name = NULL, dependencies = NULL) {
  if (is.null(obj_name))
    obj_name <- deparse(substitute(expr))
  obj_deps <- if (!is.null(dependencies)) dependencies else get_object_dependencies(obj_name = obj_name, script_path)
  if (is.null(obj_deps)) return(NULL)
  identify_inputs(obj_deps, obj_name=obj_name)
}

#' Identify input objects from a dependency list
#'
#' @param dependencies List of ScriptNodeInfo objects
#' @param dependencies (Optional) name of a focal object
#' @return Character vector of input object names
#' @keywords internal
identify_inputs <- function(dependencies, obj_name=NULL) {
  if(!is.null(obj_name)){
    init <- is_initialized(obj_name, dependencies)
    remove <- if (init) obj_name else character(0)
  }else{
    remove <- initialized_objs(dependencies)
  }
  unique(unlist(lapply(dependencies, function(x) setdiff(x@inputs, remove))))
}
#' Identify output objects from a dependency list
#'
#' @param dependencies List of ScriptNodeInfo objects
#' @return Character vector of created object names
#' @keywords internal
identify_outputs <- function(dependencies) {
  unique(c(unlist(lapply(dependencies, function(d) d@outputs)),
           unlist(lapply(dependencies, function(d) d@updates))))
}

#' Detect created objects from a dependency list
#'
#' @param dependency_list List of ScriptNodeInfo objects
#' @return Character vector of created object names
#' @keywords internal
detect_created_objects <- function(dependency_list) {
  unique(unlist(lapply(dependency_list, function(x) x@outputs)))
}

#' Get Code Dependencies for an Object
#'
#' Uses `CodeDepends` to find the statements in a script that generated a specific object.
#'
#' @param obj_name Character name of the object to trace.
#' @param script_path Path to the script file (optional). Will guess if NULL.
#' @return A list of `ScriptNodeInfo` objects or NULL.
#' @export
get_object_dependencies <- function(obj_name, script_path = NULL) {
  if (is.null(script_path)) {
    script_path <- guess_script_path()
  }
  if (is.null(script_path) || !file.exists(script_path)) {
    warning("Script path unavailable or invalid; cannot retrieve dependencies.")
    return(NULL)
  }

  info <- extract_dependencies_from_code(script_path = script_path)
  # script <- tryCatch(CodeDepends::readScript(script_path), error = function(e) {
  #   warning("Error reading script for dependency analysis: ", e$message)
  #   return(NULL)
  # })
  # if (is.null(script)) return(NULL)
  #
  # info <- CodeDepends::getInputs(script)
  deps <- tryCatch(CodeDepends::getDependsThread(obj_name, info), error = function(e) list())
  if (length(deps) == 0) return(NULL)

  info[deps]
}

#' Extract dependencies from code lines
#'
#' @param code_lines Character vector of R code lines
#' @param script_path Path to the script file
#' @return List of ScriptNodeInfo objects
#' @keywords internal
extract_dependencies_from_code <- function(code_lines = NULL, script_path=NULL) {
  requireNamespace("CodeDepends", quietly = TRUE)
  if(is.null(code_lines) & is.null(script_path)){
    stop('Error: code_lines or script_path must be specified')
  }
  if(!is.null(code_lines)){
    script <- CodeDepends::readScript(txt = code_lines)
  }else{
    script <- tryCatch(CodeDepends::readScript(script_path), error = function(e) {
      warning("Error reading script for dependency analysis: ", e$message)
      return(NULL)
    })
  }
  if (is.null(script)) return(NULL)

  info <- CodeDepends::getInputs(script)

  info
}


#' Check the consistency of the generating script with the recorded checksum.
#'
#' @param capture The capture metadata object (of class 'capture_info').
#' @param verbose Logical indicating whether to print messages (default: TRUE).
#' @return TRUE if the script is consistent with the saved checksum.
#' @export
check_capture <- function(capture, verbose = TRUE) {
  if (inherits(capture, "capture_info") &&
      !is.null(capture$script_path) &&
      file.exists(capture$script_path) &&
      !is.null(capture$script_checksum)) {

    current_checksum <- tryCatch(
      digest::digest(file = capture$script_path, algo = capture$digest_algorithm),
      error = function(e) {
        if (verbose) warning("Could not calculate current script checksum:", e$message)
        NULL
      }
    )

    if (!is.null(current_checksum)) {
      if (identical(current_checksum, capture$script_checksum)) {
        if (verbose) message("Capture check: Script is consistent with original.")
        return(TRUE)
      } else {
        if (verbose) message("Capture check: Script has changed since object was saved.")
        return(FALSE)
      }
    }
  }
  if (verbose) {
    message("Capture check: Script or checksum unavailable or invalid.")
  }
  return(FALSE)
}

#' Validate a script checksum against the capture metadata.
#'
#' @param object An object with a `capture` attribute.
#' @return TRUE if the script is unchanged, FALSE otherwise.
#' @export
capture_checksum_valid <- function(object) {
  capture <- attr(object, "capture")
  if (is.null(capture) ||
      is.null(capture$script_checksum) ||
      is.null(capture$script_path) ||
      !file.exists(capture$script_path)) {
    return(FALSE)
  }
  identical(
    digest::digest(file = capture$script_path, algo = capture$digest_algorithm),
    capture$script_checksum
  )
}

#' Export generating code from captured object.
#'
#' @param object A captured object.
#' @param file File path for output.
#' @param header Include header metadata? Default TRUE.
#' @return File path (invisibly).
#' @export
export_capture_code <- function(object, file, header = TRUE) {
  capture <- attr(object, "capture")
  if (is.null(capture) || is.null(capture$code_lines)) {
    stop("Capture metadata with generating code not found.")
  }

  code_lines <- as.character(capture$code_lines)

  if (header) {
    metadata_header <- c(
      "# ---- Reconstructed Code from Capture ----",
      paste0("# Object: ", capture$object_name),
      paste0("# Timestamp: ", format(capture$timestamp)),
      paste0("# Digest (", capture$digest_algorithm, "): ", capture$object_digest),
      "# -------------------------------------------",
      ""
    )
    code_lines <- c(metadata_header, code_lines)
  }

  writeLines(code_lines, con = file)
  invisible(file)
}

#' Capture and attach metadata to multiple objects
#'
#' @param object_names Character vector of object names.
#' @param code_lines Character vector of code lines.
#' @param env Environment in which to locate objects.
#' @param script_path Optional script path.
#' @param tags Optional character vector of tags.
#' @param digest_algo Digest algorithm to use.
#' @param verbose Logical; whether to print capture summary.
#' @param include_session Logical; Should session info be included in metadata
#' @return A `capture_env` object.
#' @keywords internal
build_capture_env <- function(object_names, code_lines, env, script_path = NULL,
                              tags = NULL, digest_algo = "sha256",
                              verbose = FALSE, include_session = FALSE) {

  tryCatch({
    dependencies <- extract_dependencies_from_code(code_lines)
    meta <- compute_capture_metadata(
      object_names = object_names,
      code_lines = code_lines,
      dependencies = dependencies,
      digest_algo = digest_algo,
      env = env,
      script_path = script_path,
      tags = tags
    )

    capture <- build_capture_info(
      object_name = paste(object_names, collapse = ", "),
      timestamp = meta$timestamp,
      script_path = meta$metadata$script_path,
      code_lines = code_lines,
      dependencies = dependencies,
      script_checksum = meta$metadata$script_checksum,
      seed = meta$seed,
      is_stochastic = is_stochastic_code(code_lines),
      session = if (include_session) sessionInfo() else NULL,
      object_digest = meta$digests,
      git_info = meta$metadata$git_info,
      tags = tags,
      package_versions = meta$package_versions,
      warnings = NULL,
      digest_algorithm = digest_algo
    )

    objects <- mget(object_names, envir = env)
    capture_env(objects, capture)

  }, error = function(e) {
    warning("Capture failed: ", e$message)
    capture_env(list(), NULL)
  })
}


#' Build capture_info metadata object
#'
#' @param object_name Name of the object.
#' @param timestamp POSIXct timestamp of capture.
#' @param script_path Path to the originating script.
#' @param code_lines Code lines used to generate the object.
#' @param dependencies List of ScriptNodeInfo objects.
#' @param script_checksum Checksum of the script file.
#' @param seed set.seed() line found in code.
#' @param session Output of sessionInfo().
#' @param object_digest Digest hash of the object.
#' @param git_info Git metadata list.
#' @param tags Optional tags.
#' @param package_versions Named character vector of package versions.
#' @param warnings List of warning strings.
#' @param digest_algorithm Digest algorithm used.
#'
#' @return A `capture_info` object.
#' @keywords internal
build_capture_info <- function(object_name, timestamp, script_path, code_lines, dependencies,
                               script_checksum, seed, session, object_digest, git_info, tags,
                               package_versions, warnings, digest_algorithm) {
  inputs <- identify_inputs(dependencies)
  outputs <- identify_outputs(dependencies)
  capture_info(
    inputs = inputs,
    outputs = outputs,
    version = capture_version(),
    object_name = object_name,
    timestamp = timestamp,
    script_path = script_path,
    code_lines = code_lines,
    dependencies = dependencies,
    script_checksum = script_checksum,
    seed = seed,
    session = session,
    object_digest = object_digest,
    git_info = git_info,
    tags = tags,
    package_versions = package_versions,
    warnings = warnings,
    digest_algorithm = digest_algorithm
  )
}

#' Compute capture metadata
#'
#' @param object_names Character vector of object names.
#' @param code_lines Character vector of R code lines.
#' @param dependencies List of ScriptNodeInfo objects.
#' @param digest_algo Digest algorithm to use.
#' @param env Environment to look up objects in.
#' @param script_path Optional script path.
#' @param tags Optional character vector of tags.
#'
#' @return Named list containing digests, seed, versions, and script metadata.
#' @keywords internal
compute_capture_metadata <- function(object_names, code_lines, dependencies, digest_algo, env, script_path = NULL, tags = NULL) {
  seed_value <- get_code_seed(code_lines)
  digests <- compute_object_digests(object_names, env = env, algo = digest_algo)
  package_versions <- if (!is.null(dependencies)) get_package_versions(dependencies) else NULL

  metadata <- if (!is.null(script_path)) get_script_metadata(script_path, algo = digest_algo) else list(
    script_path = NULL,
    script_checksum = NULL,
    timestamp = Sys.time(),
    git_info = NULL
  )

  list(
    seed = seed_value,
    digests = digests,
    package_versions = package_versions,
    metadata = metadata,
    timestamp = metadata$timestamp
  )
}


#' Attach capture metadata to objects
#'
#' @param objects Named list of objects.
#' @param capture A `capture_info` object.
#'
#' @return List of objects with capture attribute set.
#' @keywords internal
attach_capture_to_objects <- function(objects, capture) {
  for (obj in names(objects)) {
    attr(objects[[obj]], "capture") <- capture
  }
  objects
}


#' Get Git Metadata for a Script
#'
#' Returns Git commit information for the script's repository, if available.
#'
#' @param script_path Path to the script file.
#' @return A named list with commit, author, message, and date; or NULL.
#' @export
get_git_info <- function(script_path) {
  if (!requireNamespace("gert", quietly = TRUE)) return(NULL)
  repo <- tryCatch(gert::git_find(path = script_path), error = function(e) NULL)
  if (is.null(repo)) return(NULL)
  commit <- tryCatch(gert::git_log(repo = repo, max = 1)[1, ], error = function(e) NULL)
  if (is.null(commit)) return(NULL)
  list(
    commit = commit$id,
    author = commit$author,
    message = commit$message,
    date = as.character(commit$time)
  )
}

#' Guess the Current Script Path
#'
#' Attempts to determine the path of the currently executing R script or notebook.
#' Works in RStudio, `.Rhistory`, `source()`, and `knitr` contexts.
#'
#' @param verbose Logical; if TRUE, prints detailed messages.
#' @return Character string path, "Notebook or R Markdown Execution", or NULL if undetectable.
#' @keywords internal
guess_script_path <- function(verbose = FALSE) {
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable() &&
      rstudioapi::hasFun("getSourceEditorContext")) {
    context <- rstudioapi::getSourceEditorContext()
    if (nzchar(context$path)) {
      if (verbose) message("Script path detected from active RStudio document.")
      return(context$path)
    }
  }

  if (requireNamespace("knitr", quietly = TRUE) &&
      knitr::is_latex_output()) {
    if (verbose) message("Script appears to be running inside a notebook or R Markdown.")
    return("Notebook or R Markdown Execution")
  }

  calls <- sys.calls()
  for (i in seq_along(calls)) {
    call <- calls[[i]]
    if (is.call(call) && identical(call[[1]], as.name("source"))) {
      if (verbose) message("Script path detected from sys.calls().")
      return(as.character(call[[2]]))
    }
  }

  history_file <- path.expand("~/.Rhistory")
  history <- tryCatch(readLines(history_file), error = function(e) NULL)
  if (!is.null(history)) {
    sourced_lines <- grep("^source\\(['\"](.+?)['\"]\\)", history, value = TRUE)
    if (length(sourced_lines) > 0) {
      last_sourced <- tail(sourced_lines, 1)
      path <- gsub("^source\\(['\"](.+?)['\"]\\).*", "\\1", last_sourced)
      if (file.exists(normalizePath(path, mustWork = FALSE))) {
        if (verbose) message("Script path retrieved from history via source().")
        return(path)
      }
    }
  }

  if (verbose) message("Could not automatically determine script path.")
  return(NULL)
}
