rm(list = ls())

# Notes -------------------------------------------------------------------
  
  #########################################################################
  # This script runs all R scripts in the order in which they need to be  #
  # run to reproduce the analyses reported in the manuscript.             #
  #########################################################################

# Prepare -----------------------------------------------------------------

  # Screen records for eligibility
  source("scripts/screen_records.R")

  # Code records for eligibility
  source("scripts/code_records.R")

  # Extract information about studies and measures
  source("scripts/extract_study_information.R")

  # Extract effect sizes
  source("scripts/extract_effect_sizes.R")

  # Code moderators
  source("scripts/code_moderators.R")

  # Code moderators related to predictor/outcome measures
  source("scripts/code_measures.R")

  # Combine and prepare data for analyses
  source("scripts/prepare_data_for_analyses.R")


# Estimate ----------------------------------------------------------------

  # Run preregistered analyses
  source("scripts/run_preregistered_analyses.R")

  # Run preregistered robustness checks
  source("scripts/run_preregistered_robustness_checks.R")
