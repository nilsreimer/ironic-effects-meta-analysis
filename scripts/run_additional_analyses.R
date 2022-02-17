rm(list = ls())

# Notes -------------------------------------------------------------------

  #########################################################################
  # Adjust the number of cores to suit your computing environment (e.g.,  #
  # run parallel::detectCores() to find the number of available cores).   #
  #########################################################################

  #########################################################################
  # This is an additional analyses, requested by a reviewer, to test a    #
  # strong version of the 'ironic' effects hypothesis (qualitatively      #
  # positive contact between adults).                                     #
  #########################################################################

# Library -----------------------------------------------------------------

  # Load packages
  library(tidyverse); library(rstan); library(tidybayes); library(irr)

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
  
  # Exclude data to test 'strong' version of hypothesis
  dl <- dl %>% 
    filter(
      !str_detect(age, "Children|Adolescents"),
      ic_direct == "Directly",
      ic_quality == "Yes"
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
    group_by(id, sample, x, y, x_var, y_var) %>% 
    summarise(
      n = unique(n), 
      r = mean(r, na.rm = TRUE)
    ) %>% 
    ungroup() %>% 
    select(id, sample, n, r, everything())
  
  
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
  
  # Run model
  pi_fit <- sampling(
    model,
    data = pi_dlist,
    control = list(adapt_delta = 0.95),
    chains = n_cores,
    iter = n_warmup + n_iter/n_cores,
    warmup = n_warmup,
    seed = seeds[1]
  )
  
  # Check
  stan_rhat(pi_fit)
  stan_trace(pi_fit, pars = c("r_mean", "tau_ii", "tau_jj"))
  check_hmc_diagnostics(pi_fit)
  print(pi_fit, pars = c("r_mean", "tau_ii", "tau_jj"))
  paste("Pr (r < 0) =", round(mean(as.data.frame(pi_fit, "r_mean")$r_mean < 0), 3))

  
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
  
  # Run model
  ca_fit <- sampling(
    model,
    data = ca_dlist,
    control = list(adapt_delta = 0.95),
    chains = n_cores,
    iter = n_warmup + n_iter/n_cores,
    warmup = n_warmup,
    seed = seeds[2]
  )
  
  # Check
  stan_rhat(ca_fit)
  stan_trace(ca_fit, pars = c("r_mean", "tau_ii", "tau_jj"))
  check_hmc_diagnostics(ca_fit)
  print(ca_fit, pars= c("r_mean", "tau_ii", "tau_jj"))
  paste("Pr (r < 0) =", round(mean(as.data.frame(ca_fit, "r_mean")$r_mean < 0), 3))


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
  
  # Run model
  ps_fit <- sampling(
    model,
    data = ps_dlist,
    control = list(adapt_delta = 0.99),
    chains = n_cores,
    iter = n_warmup + n_iter/n_cores,
    warmup = n_warmup,
    seed = seeds[3]
  )
  
  # Check
  stan_rhat(ps_fit)
  stan_trace(ps_fit, pars = c("r_mean", "tau_ii", "tau_jj"))
  check_hmc_diagnostics(ps_fit)
  print(ps_fit, pars = c("r_mean", "tau_ii", "tau_jj"))
  paste("Pr (r < 0) =", round(mean(as.data.frame(ps_fit, "r_mean")$r_mean < 0), 3))


# Compile -----------------------------------------------------------------

  # Merge posterior draws from models
  post <- bind_rows(
    pi_fit %>% 
      spread_draws(r_mean, mu, tau_ii, tau_jj) %>% 
      mutate(x_var = "ic", y_var = "pi"),
    ca_fit %>% 
      spread_draws(r_mean, mu, tau_ii, tau_jj) %>% 
      mutate(x_var = "ic", y_var = "ca"),
    ps_fit %>% 
      spread_draws(r_mean, mu, tau_ii, tau_jj) %>% 
      mutate(x_var = "ic", y_var = "ps")
  )
  
  # Add sample information
  post <- left_join(
    post, 
    es %>% 
      group_by(x_var, y_var) %>% 
      summarize(I = n(), J = n_distinct(id), N = sum(n)) %>% 
      ungroup(), 
    by = c("x_var", "y_var")
  )

  # Extract study-wise estimates
  pred_jj <- bind_rows(
    pi_fit %>% 
      spread_draws(mu, tau_jj, b_jj[jj]) %>% 
      mutate(x_var = "ic", y_var = "pi"),
    ca_fit %>% 
      spread_draws(mu, tau_jj, b_jj[jj]) %>% 
      mutate(x_var = "ic", y_var = "ca"),
    ps_fit %>% 
      spread_draws(mu, tau_jj, b_jj[jj]) %>% 
      mutate(x_var = "ic", y_var = "ps")
  ) %>% 
    mutate(
      r_pred = z_to_r(mu + b_jj*tau_jj)
    ) %>% 
    select(-mu, -tau_jj, -b_jj) %>% 
    ungroup() %>% 
    left_join(
      es %>% 
        group_by(y_var) %>% 
        mutate(jj = as.integer(factor(id))) %>% 
        ungroup() %>% 
        distinct(id, jj, x_var, y_var),
      by = c("x_var", "y_var", "jj")
    )

# Export ------------------------------------------------------------------

  # Export results (as .rds)
  write_rds(post, "results/results_additional_analyses.rds")
  
  # Export study-wise estimates (as .rds)
  write_rds(pred_jj, "results/r_additional_analyses_pred_jj.rds")
