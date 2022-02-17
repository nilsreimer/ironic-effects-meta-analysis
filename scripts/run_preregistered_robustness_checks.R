rm(list = ls())

# Notes -------------------------------------------------------------------

  #########################################################################
  # Adjust the number of cores to suit your computing environment (e.g.,  #
  # run parallel::detectCores() to find the number of available cores).   #
  #########################################################################

# Library -----------------------------------------------------------------

  # Load packages
  library(tidyverse); library(rstan); library(tidybayes)

  # Stan options
  n_cores  <- 8
  n_iter   <- 8000
  n_warmup <- 1000
  seeds    <- c(6967674, 5768664, 7517370)
  options(mc.cores = n_cores)
  rstan_options(auto_write = FALSE)

  # Link functions
  r_to_z <- function(r) 0.5 * log( (1 + r) / (1 - r) )
  z_to_r <- function(z) ( exp(2 * z) - 1 ) / ( exp(2 * z) + 1 )

  
# Prepare -----------------------------------------------------------------
  
  # Load (preregistered) model
  model <- stan_model("models/2l-meta-analysis.stan")
  
  # Import data
  dl <- read_rds("data/dl.rds")
  
  # Select outcomes
  es <- dl %>% 
    filter(x_var == "ic", y_var %in% c("pi", "ca", "ps")) %>% 
    group_by(id, sample, y_var) %>% 
    top_n(1, -x_rank) %>%
    top_n(1, -y_rank) %>% 
    ungroup()
  
  # Average effect sizes for multiple (equivalent) outcomes
  es <- es %>% 
    group_by(id, sample, x, y, x_var, y_var) %>% 
    summarise(
      n = unique(n), 
      r = mean(r, na.rm = TRUE)
    ) %>% 
    ungroup() %>% 
    select(id, sample, n, r, everything())
  
  # Import results from preregistered analyses
  post <- read_rds("results/results_preregistered_analyses.rds")


# Estimate (perceived injustice) ------------------------------------------
  
  # Prepare data list
  pi_dlist <- es %>%
    filter(y_var == "pi") %>%
    transmute(
      ii = 1:n(),
      jj = as.integer(factor(id)),
      r  = r,
      n  = n
    ) %>%
    group_by(jj) %>%
    mutate(
      n_sample = n(),
      ii = if_else(n_sample == 1L, 0L, ii)
    ) %>%
    ungroup() %>%
    mutate(ii = as.integer(factor(ii)) - 1) %>%
    with(., list(
      I  = length(ii),
      J  = max(jj),
      ii = ii,
      jj = jj,
      r  = r,
      n  = n
    ))
  
  # Run model with narrow prior
  pi_fit_narrow <- stan(
    "models/2l-meta-analysis-narrower-prior.stan",
    data = pi_dlist,
    control = list(adapt_delta = 0.95),
    chains = n_cores,
    iter = n_warmup + n_iter/n_cores,
    warmup = n_warmup,
    seed = seeds[1]
  )
  
  # Run model with wide prior
  pi_fit_wide <- stan(
    "models/2l-meta-analysis-wider-prior.stan",
    data = pi_dlist,
    control = list(adapt_delta = 0.95),
    chains = n_cores,
    iter = n_warmup + n_iter/n_cores,
    warmup = n_warmup,
    seed = seeds[1]
  )

  
# Estimate (collective action) --------------------------------------------
  
  # Prepare data list
  ca_dlist <- es %>%
    filter(y_var == "ca") %>%
    transmute(
      ii = 1:n(),
      jj = as.integer(factor(id)),
      r  = r,
      n  = n
    ) %>%
    group_by(jj) %>%
    mutate(
      n_sample = n(),
      ii = if_else(n_sample == 1L, 0L, ii)
    ) %>%
    ungroup() %>%
    mutate(ii = as.integer(factor(ii)) - 1) %>%
    with(., list(
      I  = length(ii),
      J  = max(jj),
      ii = ii,
      jj = jj,
      r  = r,
      n  = n
    ))
  
  # Run model with narrow prior
  ca_fit_narrow <- stan(
    "models/2l-meta-analysis-narrower-prior.stan",
    data = ca_dlist,
    control = list(adapt_delta = 0.95),
    chains = n_cores,
    iter = n_warmup + n_iter/n_cores,
    warmup = n_warmup,
    seed = seeds[2]
  )

  # Run model with wide prior
  ca_fit_wide <- stan(
    "models/2l-meta-analysis-wider-prior.stan",
    data = ca_dlist,
    control = list(adapt_delta = 0.95),
    chains = n_cores,
    iter = n_warmup + n_iter/n_cores,
    warmup = n_warmup,
    seed = seeds[2]
  )
  
  
# Estimate (policy support) -----------------------------------------------
  
  # Prepare data list
  ps_dlist <- es %>%
    filter(y_var == "ps") %>%
    transmute(
      ii = 1:n(),
      jj = as.integer(factor(id)),
      r  = r,
      n  = n
    ) %>%
    group_by(jj) %>%
    mutate(
      n_sample = n(),
      ii = if_else(n_sample == 1L, 0L, ii)
    ) %>%
    ungroup() %>%
    mutate(ii = as.integer(factor(ii)) - 1) %>%
    with(., list(
      I  = length(ii),
      J  = max(jj),
      ii = ii,
      jj = jj,
      r  = r,
      n  = n
    ))
  
  # Run model with narrow prior
  ps_fit_narrow <- stan(
    "models/2l-meta-analysis-narrower-prior.stan",
    data = ps_dlist,
    control = list(adapt_delta = 0.99),
    chains = n_cores,
    iter = n_warmup + n_iter/n_cores,
    warmup = n_warmup,
    seed = seeds[3]
  )
  
  # Run model with wide prior
  ps_fit_wide <- stan(
    "models/2l-meta-analysis-wider-prior.stan",
    data = ps_dlist,
    control = list(adapt_delta = 0.99),
    chains = n_cores,
    iter = n_warmup + n_iter/n_cores,
    warmup = n_warmup,
    seed = seeds[3]
  )
  

# Check for influential priors --------------------------------------------

  # Combine
  d_priors <- bind_rows(
      pi_fit_narrow %>% 
        spread_draws(r_mean) %>% 
        mutate(y_var = "pi", prior = "N(0, 0.1)"),
      pi_fit_wide %>% 
        spread_draws(r_mean) %>% 
        mutate(y_var = "pi", prior = "N(0, 1.0)"),
      ca_fit_narrow %>% 
        spread_draws(r_mean) %>% 
        mutate(y_var = "ca", prior = "N(0, 0.1)"),
      ca_fit_wide %>% 
        spread_draws(r_mean) %>% 
        mutate(y_var = "ca", prior = "N(0, 1.0)"),
      ps_fit_narrow %>% 
        spread_draws(r_mean) %>% 
        mutate(y_var = "ps", prior = "N(0, 0.1)"),
      ps_fit_wide %>% 
        spread_draws(r_mean) %>% 
        mutate(y_var = "ps", prior = "N(0, 1.0)")
    )
  
  # Merge with results from preregistered analyses
  d_priors <- post %>% 
    select(.chain:.draw, x_var, y_var, r_mean) %>% 
    left_join(
      d_priors %>% mutate(x_var = "ic") %>% rename(r_prior = r_mean), 
      .,
      by = c(".chain", ".iteration", ".draw", "x_var", "y_var")
    )
  
  # Calculate differences
  d_priors <- d_priors %>% 
    mutate(d_r_mean = r_mean - r_prior) %>% 
    select(.chain, .iteration, .draw, y_var, prior, r_mean = r_prior, d_r_mean)


# Check for influential studies -------------------------------------------

  # Perceived injustice
  for (j in unique(es$id[es$y_var == "pi"])) {
    
    # Prepare data list
    pi_dlist <- es %>%
      filter(id != j, y_var == "pi") %>%
      transmute(
        ii = 1:n(),
        jj = as.integer(factor(id)),
        r  = r,
        n  = n
      ) %>%
      group_by(jj) %>%
      mutate(
        n_sample = n(),
        ii = if_else(n_sample == 1L, 0L, ii)
      ) %>%
      ungroup() %>%
      mutate(ii = as.integer(factor(ii)) - 1) %>%
      with(.,list(
        I  = length(ii),
        J  = max(jj),
        ii = ii,
        jj = jj,
        r  = r,
        n  = n
      ))
    
    # Run model
    pi_loo_fit <- sampling(
      model,
      data = pi_dlist,
      control = list(adapt_delta = 0.95),
      chains = n_cores,
      iter = n_warmup + n_iter/n_cores,
      warmup = n_warmup,
      seed = seeds[1]
    )
    
    # Extract estimate
    if (j == min(unique(es$id[es$y_var == "pi"]))) {
      pi_loo <- pi_loo_fit %>% spread_draws(r_mean) %>% mutate(id = j)
    } else {
      pi_loo <- bind_rows(
        pi_loo,
        pi_loo_fit %>% spread_draws(r_mean) %>% mutate(id = j) %>% select(id, everything())
      )
    }
    
    # Print intermediate results
    pi_loo %>% arrange(desc(id)) %>% print(n = 5)
    
  }
  
  # Collective actions
  for (j in unique(es$id[es$y_var == "ca"])) {
    
    # Prepare data list
    ca_dlist <- es %>%
      filter(id != j, y_var == "ca") %>%
      transmute(
        ii = 1:n(),
        jj = as.integer(factor(id)),
        r  = r,
        n  = n
      ) %>%
      group_by(jj) %>%
      mutate(
        n_sample = n(),
        ii = if_else(n_sample == 1L, 0L, ii)
      ) %>%
      ungroup() %>%
      mutate(ii = as.integer(factor(ii)) - 1) %>%
      with(.,list(
        I  = length(ii),
        J  = max(jj),
        ii = ii,
        jj = jj,
        r  = r,
        n  = n
      ))
    
    # Run model
    ca_loo_fit <- sampling(
      model,
      data = ca_dlist,
      control = list(adapt_delta = 0.95),
      chains = n_cores,
      iter = n_warmup + n_iter/n_cores,
      warmup = n_warmup,
      seed = seeds[2]
    )
    
    # Extract estimate
    if (j == min(unique(es$id[es$y_var == "ca"]))) {
      ca_loo <- ca_loo_fit %>% spread_draws(r_mean) %>% mutate(id = j)
    } else {
      ca_loo <- bind_rows(
        ca_loo,
        ca_loo_fit %>% spread_draws(r_mean) %>% mutate(id = j) %>% select(id, everything())
      )
    }
    
    # Print intermediate results
    ca_loo %>% arrange(desc(id)) %>% print(n = 5)
    
  }
  
  # Policy support
  for (j in unique(es$id[es$y_var == "ps"])) {
    
    # Prepare data list
    ps_dlist <- es %>%
      filter(id != j, y_var == "ps") %>%
      transmute(
        ii = 1:n(),
        jj = as.integer(factor(id)),
        r  = r,
        n  = n
      ) %>%
      group_by(jj) %>%
      mutate(
        n_sample = n(),
        ii = if_else(n_sample == 1L, 0L, ii)
      ) %>%
      ungroup() %>%
      mutate(ii = as.integer(factor(ii)) - 1) %>%
      with(.,list(
        I  = length(ii),
        J  = max(jj),
        ii = ii,
        jj = jj,
        r  = r,
        n  = n
      ))
    
    # Run model
    ps_loo_fit <- sampling(
      model,
      data = ps_dlist,
      control = list(adapt_delta = 0.99),
      chains = n_cores,
      iter = n_warmup + n_iter/n_cores,
      warmup = n_warmup,
      seed = seeds[3]
    )
    
    # Extract estimate
    if (j == min(unique(es$id[es$y_var == "ps"]))) {
      ps_loo <- ps_loo_fit %>% spread_draws(r_mean) %>% mutate(id = j)
    } else {
      ps_loo <- bind_rows(
        ps_loo,
        ps_loo_fit %>% spread_draws(r_mean) %>% mutate(id = j) %>% select(id, everything())
      )
    }
    
    # Print intermediate results
    ps_loo %>% arrange(desc(id)) %>% print(n = 5)
    
  }
  
  # Combine
  ae_loo <- bind_rows(
      pi_loo %>% mutate(x_var = "ic", y_var = "pi"),
      ca_loo %>% mutate(x_var = "ic", y_var = "ca"),
      ps_loo %>% mutate(x_var = "ic", y_var = "ps")
    ) %>% 
    rename(r_loo = r_mean)
  
  # Merge with results from preregistered analyses
  ae_loo <- post %>% 
    select(.chain:.draw, x_var, y_var, r_mean) %>% 
    left_join(
      ae_loo, 
      .,
      by = c(".chain", ".iteration", ".draw", "x_var", "y_var")
    )

  # Calculate (absolute) error
  ae_loo <- ae_loo %>% 
    mutate(
      e_loo = r_mean - r_loo,
      ae_loo = abs(r_mean - r_loo)
    ) %>% 
    select(.chain, .iteration, .draw, y_var, id, r_loo, e_loo, ae_loo)
  

# Export ------------------------------------------------------------------

  # Export prior choice results (as .rds)
  write_rds(d_priors, "results/results_robustness_checks_priors.rds")
  
  # Export leave-one-out results (as .rds)
  write_rds(ae_loo, "results/results_robustness_checks_loo.rds")
