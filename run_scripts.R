rm(list = ls())

# Notes -------------------------------------------------------------------
  
  #########################################################################
  # This script runs all R scripts in the order in which they need to be  #
  # run to reproduce the analyses reported in the manuscript. You might   #
  # need to restart R between scripts.                                    #
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
  source("scripts/run_meta_bias_analyses.R")
  

# Visualize ---------------------------------------------------------------
  
  # Make figures
  source("figures/figure-1.R")
  source("figures/figure-2.R")
  source("figures/figure-3.R")
  source("figures/figure-4.R")
  source("figures/figure-5.R")
  source("figures/figure-6.R")
  source("figures/figure-7.R")
  source("figures/figure-8.R")
  source("figures/figure-s1.R")
  source("figures/figure-s2.R")

# Combine -----------------------------------------------------------------

  # Make manuscript
  source("manuscript/manuscript.Rmd")

  # Make supplemental materials
  source("manuscript/supplemental-materials.Rmd")
