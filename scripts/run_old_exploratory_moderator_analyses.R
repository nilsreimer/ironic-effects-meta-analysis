rm(list = ls())

# Notes -------------------------------------------------------------------

  #########################################################################
  # Adjust the number of cores to suit your computing environment (e.g.,  #
  # run parallel::detectCores() to find the number of available cores).   #
  #########################################################################

# Library -----------------------------------------------------------------

  # Load packages
  library(tidyverse); library(rstan); library(tidybayes); library(metacart)

  # Stan options
  n_cores  <- 8
  n_iter   <- 8000
  n_warmup <- 1000
  options(mc.cores = n_cores)
  rstan_options(auto_write = TRUE)
  
  # Link functions
  r_to_z <- function(r) 0.5 * log( (1 + r) / (1 - r) )
  z_to_r <- function(z) ( exp(2 * z) - 1 ) / ( exp(2 * z) + 1 )

  
# Prepare -----------------------------------------------------------------
  
  # Import data
  dl <- read_rds("data/dl.rds")
  
  # Recode variables
  dl <- dl %>% 
    mutate(
      age = case_when(
        str_detect(age, "Adults") & !str_detect(age, "Adolescents|Children") ~ "Adults",
        str_detect(age, "Adolescents|Children") ~ "Adolescents/Children"
      ),
      publication_status = case_when(
        publication_status == "published" ~ "published",
        str_detect(publication_status, "unpublished") ~ "unpublished"
      ),
      study_design = ordered(study_design, levels = c(
        "observational, cross-sectional", "observational, longitudinal", 
        "quasi-experimental (no random assignment)", "experimental (random assignment)"
      )),
      study_sample = ordered(study_sample, levels = c(
        "convenience sample, students (no random sampling)",
        "convenience sample, non-students (no random sampling)",
        "probability or representative sample"
      ))
    )
  
  # Select outcomes
  es <- dl %>% 
    filter(x_var == "ic", y_var %in% c("pi", "ca", "ps")) %>% 
    group_by(id, sample, y_var) %>% 
    top_n(1, -x_rank) %>%
    top_n(1, -y_rank) %>% 
    ungroup()
  
  # Average effect sizes for multiple (equivalent) outcomes
  es <- es %>% 
    group_by(id, sample, x, y, x_var, y_var, ic_direct, pi_specific, pi_personal) %>% 
    summarise(
      n = unique(n), 
      r = mean(r, na.rm = TRUE)
    ) %>% 
    ungroup() %>% 
    select(id, sample, n, r, everything())
  
  # Create indices
  es <- es %>% 
    group_by(y_var) %>% 
    mutate(
      ii = 1:n(),
      jj = as.integer(factor(id))
    ) %>% 
    group_by(y_var, jj) %>% 
    mutate(
      n_sample = n(),
      ii = if_else(n_sample == 1L, 0L, ii)
    ) %>% 
    ungroup() %>%
    mutate(ii = as.integer(factor(ii)) - 1) %>% 
    select(-n_sample)
  
  # Convert effect sizes for analyses
  es <- es %>% mutate(z = r_to_z(r), vi = 1 / (n - 3))

  # Add (categorical) moderators
  es <- dl %>% 
    distinct(
      id, sample, x, y, 
      study_setting, study_design, study_sample, age,
      study_intention, publication_status
    ) %>% 
    left_join(es, ., by = c("id", "sample", "x", "y"))
  

# Estimate (perceived injustice) ------------------------------------------
  
  # Set seed
  set.seed(1377042)
  
  # Run random-effects meta-tree model
  pi_tree <- REmrt(
    z ~ pi_specific + pi_personal + ic_direct + study_setting + study_design + study_sample + age + study_intention + publication_status,
    vi = vi,
    data = es %>% filter(y_var == "pi"),
    c = 0.0,
    xval = nrow(es %>% filter(y_var == "pi")),
    lookahead = TRUE
  )
  
  # Add categories to data
  pi_es <- es %>% 
    filter(y_var == "pi") %>% 
    mutate(
      ii = row_number(),
      kk = case_when(
        age != "Adults" ~ 1L,
        ic_direct != "Directly" ~ 2L,
        !(study_setting %in% c("Colonization", "Short-term migration")) ~ 3L,
        study_setting %in% c("Colonization", "Short-term migration") ~ 4L
      )
    )
  
  # Run Bayesian random-effects model
  pi_fit <- stan(
    "models/1l-meta-analysis-categorical-moderators.stan",
    data = with(pi_es, list(
      I = max(ii),
      K = max(kk),
      ii = ii,
      kk = kk,
      r = r,
      n = n
    )),
    control = list(adapt_delta = 0.95, max_treedepth = 12),
    chains = n_cores,
    iter = n_warmup + n_iter/n_cores,
    warmup = n_warmup,
    seed = 1377042
  )
  
  # Extract posterior samples
  results <- pi_es %>% 
    mutate(
      study_setting = case_when(
        age != "Adults" | ic_direct == "Indirectly" ~ NA_character_, 
        str_detect(study_setting, "Colonization|Short-term migration") ~ "Colonization/Short-term migration",
        TRUE ~ "Other"
      ),
      ic_direct = case_when(
        age != "Adults" ~ NA_character_,
        TRUE ~ ic_direct
      )
    ) %>% 
    count(kk, age, ic_direct, study_setting) %>% 
    full_join(
      pi_fit %>% spread_draws(r_kk[kk], R2),
      by = "kk"
    ) %>% 
    select(.chain:.draw, kk, r = r_kk, n, R2, age:study_setting)
  
  
# Estimate (collective action) --------------------------------------------

  # Set seed
  set.seed(3242844)
  
  # Run random-effects meta-tree model
  ca_tree <- REmrt(
    z ~ ic_direct + study_setting + study_design + study_sample + age + study_intention + publication_status,
    vi = vi,
    data = es %>% filter(y_var == "ca"),
    c = 0.0,
    xval = nrow(es %>% filter(y_var == "ca")),
    lookahead = TRUE
  )
  

# Estimate (policy support) -----------------------------------------------

  # Set seed
  set.seed(1289771)
  
  # Run random-effects meta-tree model
  ps_tree <- REmrt(
    z ~ ic_direct + study_setting + study_design + study_sample + age + study_intention + publication_status,
    vi = vi,
    data = es %>% filter(y_var == "ps"),
    c = 0.0,
    xval = nrow(es %>% filter(y_var == "ps")),
    lookahead = TRUE
  )


# Export ------------------------------------------------------------------

  # Export results (as .rds)
  write_rds(results, "results/results_old_exploratory_moderator_analyses.rds")
  