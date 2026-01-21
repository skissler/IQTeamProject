# code/setup.R
# Consolidated setup script - source this once at the start of any analysis
#
# Usage:
#   source('code/setup.R')
#
# This will:
#   1. Load all required packages
#   2. Load configuration (paths, parameters)
#   3. Load helper functions
#   4. Load epidemic models
#   5. Validate the environment (optional, set validate = FALSE to skip)

# ==============================================================================
# Load Required Packages
# ==============================================================================

library(tidyverse)
library(odin)
library(sf)
library(tigris)
library(tidycensus)
library(haven)
library(future.apply)

# Configure tigris to cache downloaded shapefiles
options(tigris_use_cache = TRUE)

# ==============================================================================
# Load Configuration
# ==============================================================================

source('code/config.R')

# ==============================================================================
# Load Census API Key
# ==============================================================================

# Attempt to load from .Renviron
readRenviron("~/.Renviron")
census_key <- Sys.getenv("CENSUS_API_KEY")

if (nchar(census_key) > 0) {
  census_api_key(census_key)
} else {
  warning("CENSUS_API_KEY not found in ~/.Renviron. ACS data import will fail.")
}

# ==============================================================================
# Load Helper Functions and Models
# ==============================================================================

source('code/utils_documented.R')
source('code/epimodels.R')

# ==============================================================================
# Optional: Validate Environment
# ==============================================================================

# Set validate = FALSE before sourcing this file to skip validation
if (!exists("validate") || validate != FALSE) {
  source('code/setup_check.R')
  check_setup(verbose = FALSE)
}

message("Setup complete. Configuration loaded from code/config.R")
