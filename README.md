# captuR

**Lightweight, Transparent R Object Capture for Reproducibility**

`captuR` is an R package for tracking how objects are created, including their generating code, dependencies, script path, Git commit, digests, seeds, session info, and more. It enables reproducibility, auditing, debugging, and regeneration of analytical workflows—without requiring custom pipelines.

## Key Features

- **Code capture**: Automatically extract and attach the code that generated any object.
- **Dependency tracing**: Use `CodeDepends` to track inputs and outputs.
- **Script and Git metadata**: Store file paths, checksums, and Git commit info.
- **Digest-based validation**: Hash and verify objects and scripts.
- **Session and seed capture**: Record session info and `set.seed()` state.
- **Rich environment capture**: Capture all outputs from a script or block as a structured object.
- **Comparison and diffing**: Compare metadata or rerun and validate code.

## Main Functions

```r
capture_object(obj)          # Capture metadata for an object in memory
capture_save(obj, "file.rds") # Save with metadata
capture_load("file.rds")     # Load with metadata and optional checksum validation

capture_block({              # Capture inline code block
  x <- rnorm(10)
  y <- mean(x)
})

capture_source("script.R")   # Run a script and capture all top-level outputs
```

## Metadata Access

```r
get_capture(obj)              # Get capture metadata
capture_script_path(obj)      # Extract script path
capture_digest(obj)           # Get object digest
capture_tags(obj)             # List user-defined tags
view_capture_code(obj)        # Print generating code
view_capture_script(obj)      # View full annotated script
```

## Comparison Tools

```r
capture_diff(obj1, obj2)      # Compare capture metadata
check_captured_code(obj)      # Re-run and verify outputs match
```

## Installation

```r
# Not on CRAN yet
# install.packages("devtools")
devtools::install_github("m-a-schultz/captuR")
```

## License

MIT
