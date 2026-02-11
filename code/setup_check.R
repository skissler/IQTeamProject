# code/setup_check.R
# Environment validation script
# Run this before analysis to verify all dependencies and data files are in place
#
# Usage:
#   source('code/setup_check.R')
#   check_setup()              # Full validation with verbose output
#   check_setup(verbose=FALSE) # Silent validation (errors only)

#' Validate that the analysis environment is correctly configured
#'
#' Checks for required data files, Census API key, output directories,
#' and required packages. Provides helpful error messages for missing components.
#'
#' @param verbose Logical. If TRUE (default), prints status for each check.
#'   If FALSE, only prints errors.
#' @return Invisible TRUE if all checks pass. Stops with error if critical
#'   components are missing.
#'
#' @examples
#' check_setup()           # Full output
#' check_setup(verbose=FALSE)  # Errors only
check_setup <- function(verbose = TRUE) {

  errors <- character(0)
  warnings <- character(0)

  # --------------------------------------------------------------------------
  # Check 1: Census API Key
  # --------------------------------------------------------------------------
  census_key <- Sys.getenv("CENSUS_API_KEY")
  if (nchar(census_key) == 0) {
    errors <- c(errors, paste(
      "Missing CENSUS_API_KEY.",
      "Add the following line to ~/.Renviron:",
      "  CENSUS_API_KEY=your_key_here",
      "Get a free key at: https://api.census.gov/data/key_signup.html",
      sep = "\n"
    ))
  } else if (verbose) {
    message("+ Census API key found")
  }

  # --------------------------------------------------------------------------
  # Check 2: Required Data Files
  # --------------------------------------------------------------------------
  required_files <- list(
    "NAWS data" = "data/naws_all.sas7bdat",
    "State-region mapping" = "data/stateregion.csv",
    "State abbreviations" = "data/stateabbrev.csv"
  )

  for (name in names(required_files)) {
    path <- required_files[[name]]
    if (!file.exists(path)) {
      errors <- c(errors, paste0("Missing ", name, ": ", path))
    } else if (verbose) {
      message(paste0("+ ", name, " found"))
    }
  }

  # --------------------------------------------------------------------------
  # Check 3: Output Directories
  # --------------------------------------------------------------------------
  output_dirs <- c("output", "figures")

  for (dir in output_dirs) {
    if (!dir.exists(dir)) {
      # Create it rather than error
      dir.create(dir, recursive = TRUE)
      if (verbose) {
        message(paste0("+ Created missing directory: ", dir))
      }
    } else if (verbose) {
      message(paste0("+ Output directory exists: ", dir))
    }
  }

  # --------------------------------------------------------------------------
  # Check 4: Required Packages
  # --------------------------------------------------------------------------
  required_packages <- c(
    "tidyverse", "odin", "sf", "tigris", "tidycensus", "haven", "future.apply",
    "patchwork"
  )

  missing_packages <- character(0)
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      missing_packages <- c(missing_packages, pkg)
    }
  }

  if (length(missing_packages) > 0) {
    errors <- c(errors, paste0(
      "Missing packages: ", paste(missing_packages, collapse = ", "),
      "\nRun: install.packages(c('", paste(missing_packages, collapse = "', '"), "'))",
      "\nOr run: renv::restore()"
    ))
  } else if (verbose) {
    message("+ All required packages installed")
  }

  # --------------------------------------------------------------------------
  # Check 5: R Version (warning only)
  # --------------------------------------------------------------------------
  r_version <- paste0(R.version$major, ".", R.version$minor)
  if (verbose) {
    message(paste0("+ R version: ", r_version))
  }

  # --------------------------------------------------------------------------
  # Check 6: Working Directory
  # --------------------------------------------------------------------------
  if (!file.exists("code/config.R")) {
    errors <- c(errors, paste(
      "Working directory does not appear to be the project root.",
      "Please set your working directory to the project root folder.",
      "Current directory:", getwd(),
      sep = "\n"
    ))
  } else if (verbose) {
    message("+ Working directory is project root")
  }

  # --------------------------------------------------------------------------
  # Report Results
  # --------------------------------------------------------------------------
  if (length(warnings) > 0) {
    for (w in warnings) {
      warning(w, call. = FALSE)
    }
  }

  if (length(errors) > 0) {
    stop(
      "\n\nSetup validation failed:\n\n",
      paste(paste0("ERROR: ", errors), collapse = "\n\n"),
      "\n\nPlease fix the above issues before running the analysis.",
      call. = FALSE
    )
  }

  if (verbose) {
    message("\n=== Setup validated successfully ===\n")
  }

  invisible(TRUE)
}


#' Quick check that can be called at the start of scripts
#'
#' A minimal check that verifies the most critical requirements without
#' verbose output. Suitable for including at the top of analysis scripts.
#'
#' @return Invisible TRUE if checks pass
quick_check <- function() {
  # Check working directory
  if (!file.exists("code/config.R")) {
    stop("Please set working directory to project root before running.")
  }


  # Check Census API key
  if (Sys.getenv("CENSUS_API_KEY") == "") {
    stop("CENSUS_API_KEY not set. See code/setup_check.R for instructions.")
  }

  # Check NAWS data
  if (!file.exists("data/naws_all.sas7bdat")) {
    stop("NAWS data file not found at data/naws_all.sas7bdat")
  }

  invisible(TRUE)
}
