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
  seeds    <- c(6967674, 5768664, 7517370, 6852246, 9115239, 5279371)
  options(mc.cores = n_cores)
  rstan_options(auto_write = TRUE)
  
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


# Estimate assications between outcome variables --------------------------

  # Prepare data list
  es_outcomes <- dl %>% 
    filter(
      x_var %in% c("pi", "ca", "ps"), 
      y_var %in% c("pi", "ca", "ps"),
      x_var != y_var
    ) %>%  
    mutate(
      r = if_else(is.na(r_ac), r, r_ac)
    ) %>% 
    group_by(id, sample, x_var, y_var) %>% 
    top_n(1, -x_rank) %>%
    top_n(1, -y_rank) %>% 
    ungroup() %>% 
    group_by(id, sample, x, y, x_var, y_var) %>% 
    summarise(
      n = unique(n), 
      r = mean(r, na.rm = TRUE)
    ) %>% 
    ungroup() %>% 
    select(id, sample, n, r, everything())
  
  # Estimate association between perceived injustice and collective action
  pi_ca_fit <- es_outcomes %>% 
    filter(x_var == "pi", y_var == "ca") %>% 
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
    )) %>% 
    sampling(
      model,
      data = .,
      control = list(adapt_delta = 0.99, max_treedepth = 12),
      chains = n_cores,
      iter = n_warmup + n_iter/n_cores,
      warmup = n_warmup,
      seed = seeds[4]
    )
  
  # Check
  stan_rhat(pi_ca_fit)
  stan_trace(pi_ca_fit, pars = c("r_mean", "tau_ii", "tau_jj"))
  check_hmc_diagnostics(pi_ca_fit)
  print(pi_ca_fit, pars = c("r_mean", "tau_ii", "tau_jj"))
  paste("Pr (r < 0) =", round(mean(as.data.frame(pi_ca_fit, "r_mean")$r_mean > 0), 3))
  
  # Estimate association between perceived injustice and policy support
  pi_ps_fit <- es_outcomes %>% 
    filter(x_var == "pi", y_var == "ps") %>% 
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
    )) %>% 
    sampling(
      model,
      data = .,
      control = list(adapt_delta = 0.99, max_treedepth = 12),
      chains = n_cores,
      iter = n_warmup + n_iter/n_cores,
      warmup = n_warmup,
      seed = seeds[5]
    )
  
  # Check
  stan_rhat(pi_ps_fit)
  stan_trace(pi_ps_fit, pars = c("r_mean", "tau_ii", "tau_jj"))
  check_hmc_diagnostics(pi_ps_fit)
  print(pi_ps_fit, pars = c("r_mean", "tau_ii", "tau_jj"))
  paste("Pr (r < 0) =", round(mean(as.data.frame(pi_ps_fit, "r_mean")$r_mean > 0), 3))
  
  # Estimate association between collective action and policy support
  ca_ps_fit <- es_outcomes %>% 
    filter(x_var == "ca", y_var == "ps") %>% 
    transmute(
      ii = 1:n(),
      r  = r,
      n  = n
    ) %>%
    with(.,list(
      I  = length(ii),
      ii = ii,
      r  = r,
      n  = n
    )) %>% 
    stan(
      "models/1l-meta-analysis.stan",
      data = .,
      control = list(adapt_delta = 0.95),
      chains = n_cores,
      iter = n_warmup + n_iter/n_cores,
      warmup = n_warmup,
      seed = seeds[6]
    )
  
  # Check
  stan_rhat(ca_ps_fit)
  stan_trace(ca_ps_fit, pars = c("r_mean", "tau_ii"))
  check_hmc_diagnostics(ca_ps_fit)
  print(ca_ps_fit, pars = c("r_mean", "tau_ii"))
  paste("Pr (r < 0) =", round(mean(as.data.frame(ca_ps_fit, "r_mean")$r_mean > 0), 3))


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
      mutate(x_var = "ic", y_var = "ps"),
    pi_ca_fit %>% 
      spread_draws(r_mean, mu, tau_ii, tau_jj) %>% 
      mutate(x_var = "pi", y_var = "ca"),
    pi_ps_fit %>% 
      spread_draws(r_mean, mu, tau_ii, tau_jj) %>% 
      mutate(x_var = "pi", y_var = "ps"),
    ca_ps_fit %>% 
      spread_draws(r_mean, mu, tau_ii) %>% 
      mutate(x_var = "ca", y_var = "ps"),
  )
  
  # Add sample information
  post <- left_join(
    post, 
    bind_rows(es, es_outcomes) %>% 
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
  write_rds(post, "results/results_preregistered_analyses.rds")
  
  # Export study-wise estimates (as .rds)
  write_rds(pred_jj, "results/r_pred_jj.rds")
