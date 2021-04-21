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

  # Run meta-regression moderator analyses
  source("scripts/run_moderator_analyses.R")

  # Run meta-regression tree moderator analyses
  source("scripts/run_exploratory_moderator_analyses.R")
  
  # Run analyses with negative contact
  source("scripts/run_analyses_with_negative_contact.R")

  # Run analyses with ingroup contact
  source("scripts/run_analyses_with_ingroup_contact.R")

  # Run analyses to test and correct for publication bias
  source("scripts/run_meta_bias_ingroup_contact.R")
  

# Visualize ---------------------------------------------------------------
  
  # Make figures
  source("scripts/figure-1.R")
  source("scripts/figure-2.R")
  source("scripts/figure-3.R")
  source("scripts/figure-4.R")
  source("scripts/figure-5.R")
  source("scripts/figure-6.R")
  source("scripts/figure-7.R")
  source("scripts/figure-8.R")


# Combine -----------------------------------------------------------------

  # Make manuscript
  source("manuscript.Rmd")

  # Make supplemental materials
  source("supplemental-materials.Rmd")
