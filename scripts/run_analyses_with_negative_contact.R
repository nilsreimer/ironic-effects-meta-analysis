rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------

  # Load packages
  library(tidyverse); library(rstan); library(tidybayes)
  
  # Stan options
  n_cores  <- 8
  n_iter   <- 8000
  n_warmup <- 1000
  seeds    <- c(6967674, 5768664, 7517370, 1585380)
  options(mc.cores = n_cores)
  rstan_options(auto_write = TRUE)
  
  # Functions
  source("scripts/functions.R")
  r_to_z <- function(r) 0.5 * log( (1 + r) / (1 - r) )
  z_to_r <- function(z) ( exp(2 * z) - 1 ) / ( exp(2 * z) + 1 )
  
  
# Prepare -----------------------------------------------------------------
  
  # Import data
  dl <- read_rds("data/dl.rds") %>% filter(!is.na(n), !is.na(r))
  
  # Prepare data
  es <- left_join(
    dl %>% 
      filter(x == "pc", y_var %in% c("pi", "ca", "ps")) %>% 
      select(id, sample, n, y, y_var, y_name, r_pc = r),
    dl %>% 
      filter(x == "nc", y_var %in% c("pi", "ca", "ps")) %>% 
      select(id, sample, n, y, y_var, y_name, r_nc = r),
    by = c("id", "sample", "n", "y", "y_var", "y_name")
  ) %>% 
    left_join(
      dl %>% 
        filter(x == "pc", y == "nc") %>% 
        mutate(r = if_else(is.na(r_ac), r, r_ac)) %>% 
        select(id, sample, n, r_pc_nc = r),
      by = c("id", "sample", "n")
    ) %>% 
    filter(!is.na(r_pc), !is.na(r_nc), !is.na(r_pc_nc))
  
  # Exclude irrelevant effect sizes
  es <- dl %>% 
    left_join(es) %>% 
    filter(
      !is.na(r_pc), 
      !is.na(r_nc), 
      !is.na(r_pc_nc),
      x %in% c("pc", "nc"),
      y_var %in% c("pi", "ca", "ps")
    ) %>% 
    select(id, sample, n, r, x:y_name, r_pc:r_pc_nc)
  
  # Calculate partial correlations
  es <- es %>%
    mutate(
      r = case_when(
        x == "pc" ~ partial_r(r_pc, r_pc_nc, r_nc),
        x == "nc" ~ partial_r(r_nc, r_pc_nc, r_pc),
        TRUE ~ NA_real_
      )
    )
  
  # Select outcomes
  es <- es %>% 
    group_by(id, sample, x, y_var) %>% 
    top_n(1, -y_rank) %>% 
    ungroup()
  
  # Transform to wide format
  es <- es %>% 
    select(id, sample, n, r, x, y, y_var, y_rank, y_name, r_pc_nc) %>% 
    pivot_wider(names_from = x, names_prefix = "r_", values_from = r)
  
  # Average effect sizes for multiple (equivalent) outcomes
  es <- es %>% 
    group_by(id, sample, y, y_var) %>% 
    summarise(
      n = unique(n), 
      across(starts_with("r_"), mean)
    ) %>% 
    ungroup() %>% 
    select(id, sample, n, everything())
  
  # Load model
  model <- stan_model("models/2l-meta-analysis-with-negative-contact.stan")


# Estimate (perceived injustice) ------------------------------------------
  
  # Prepare data list
  pi_dlist <- es %>%
    filter(y_var == "pi") %>%
    transmute(
      ii = 1:n(),
      jj = as.integer(factor(id)),
      n, r_pc, r_nc, r_pc_nc
    ) %>%
    group_by(jj) %>%
    mutate(
      n_sample = n(),
      ii = if_else(n_sample == 1L, 0L, ii)
    ) %>%
    ungroup() %>% 
    mutate(ii = as.integer(factor(ii)) - 1) %>%
    with(.,list(
      I = length(ii),
      J = max(jj),
      ii = ii,
      jj = jj,
      r_pc = r_pc,
      r_nc = r_nc,
      r_pc_nc = r_pc_nc,
      n = n
    ))

  # Run model
  pi_fit <- sampling(
    model,
    data = pi_dlist,
    control = list(adapt_delta = 0.99, max_treedepth = 15),
    chains = n_cores,
    iter = n_warmup + n_iter/n_cores,
    warmup = n_warmup,
    seed = seeds[1]
  )  
  
  # Check
  stan_rhat(pi_fit)
  stan_trace(
    pi_fit, 
    pars = c("r_pc_mean", "r_nc_mean", "r_pc_nc_mean")
  )
  check_hmc_diagnostics(pi_fit)
  print(
    pi_fit, 
    pars = c(
      "r_pc_mean", "tau_pc_ii", "tau_pc_jj",
      "r_nc_mean", "tau_nc_ii", "tau_nc_jj",
      "r_pc_nc_mean", "tau_pc_nc_ii", "tau_pc_nc_jj"
    )
  )
  paste("Pr (r < 0) =", round(mean(as.data.frame(pi_fit, "r_pc_mean")$r_pc_mean < 0), 3))
  paste("Pr (r > 0) =", round(mean(as.data.frame(pi_fit, "r_nc_mean")$r_nc_mean > 0), 3))
  
  
# Estimate (collective action) --------------------------------------------
  
  # Prepare data list
  ca_dlist <- es %>%
    filter(y_var == "ca") %>%
    transmute(
      ii = 1:n(),
      jj = as.integer(factor(id)),
      n, r_pc, r_nc, r_pc_nc
    ) %>%
    group_by(jj) %>%
    mutate(
      n_sample = n(),
      ii = if_else(n_sample == 1L, 0L, ii)
    ) %>%
    ungroup() %>% 
    mutate(ii = as.integer(factor(ii)) - 1) %>%
    with(.,list(
      I = length(ii),
      J = max(jj),
      ii = ii,
      jj = jj,
      r_pc = r_pc,
      r_nc = r_nc,
      r_pc_nc = r_pc_nc,
      n = n
    ))
  
  # Run model
  ca_fit <- sampling(
    model,
    data = ca_dlist,
    control = list(adapt_delta = 0.99, max_treedepth = 15),
    chains = n_cores,
    iter = n_warmup + n_iter/n_cores,
    warmup = n_warmup,
    seed = seeds[2]
  )  
  
  # Check
  stan_rhat(ca_fit)
  stan_trace(
    ca_fit, 
    pars = c("r_pc_mean", "r_nc_mean", "r_pc_nc_mean")
  )
  check_hmc_diagnostics(ca_fit)
  print(
    ca_fit, 
    pars = c(
      "r_pc_mean", "tau_pc_ii", "tau_pc_jj",
      "r_nc_mean", "tau_nc_ii", "tau_nc_jj",
      "r_pc_nc_mean", "tau_pc_nc_ii", "tau_pc_nc_jj"
    )
  )
  paste("Pr (r < 0) =", round(mean(as.data.frame(ca_fit, "r_pc_mean")$r_pc_mean < 0), 3))
  paste("Pr (r > 0) =", round(mean(as.data.frame(ca_fit, "r_nc_mean")$r_nc_mean > 0), 3))
  
  
# Estimate (policy support) -----------------------------------------------
  
  # Prepare data list
  ps_dlist <- es %>%
    filter(y_var == "ps") %>%
    transmute(
      ii = 1:n(),
      n, r_pc, r_nc, r_pc_nc
    ) %>%
    with(.,list(
      I = length(ii),
      ii = ii,
      r_pc = r_pc,
      r_nc = r_nc,
      r_pc_nc = r_pc_nc,
      n = n
    ))
  
  # Run model
  ps_fit <- stan(
    "models/1l-meta-analysis-with-negative-contact.stan",
    control = list(adapt_delta = 0.99, max_treedepth = 15),
    data = ps_dlist,
    chains = n_cores,
    iter = n_warmup + n_iter/n_cores,
    warmup = n_warmup,
    seed = seeds[3]
  )  
  
  # Check
  stan_rhat(ps_fit)
  stan_trace(
    ps_fit, 
    pars = c("r_pc_mean", "r_nc_mean", "r_pc_nc_mean")
  )
  check_hmc_diagnostics(ps_fit)
  print(
    ps_fit, 
    pars = c(
      "r_pc_mean", "tau_pc_ii",
      "r_nc_mean", "tau_nc_ii",
      "r_pc_nc_mean", "tau_pc_nc_ii"
    )
  )
  paste("Pr (r > 0) =", round(mean(as.data.frame(ps_fit, "r_pc_mean")$r_pc_mean > 0), 3))
  paste("Pr (r > 0) =", round(mean(as.data.frame(ps_fit, "r_nc_mean")$r_nc_mean > 0), 3))
  

# Estimate association between predictors ---------------------------------

  # Prepare data list
  pc_nc_dlist <- es %>%
    distinct(id, sample, n, r_pc_nc) %>% 
    transmute(
      ii = 1:n(),
      jj = as.integer(factor(id)),
      n,
      r_pc_nc
    ) %>%
    group_by(jj) %>%
    mutate(
      n_sample = n(),
      ii = if_else(n_sample == 1L, 0L, ii)
    ) %>%
    ungroup() %>% 
    mutate(ii = as.integer(factor(ii)) - 1) %>%
    with(.,list(
      I = length(ii),
      J = max(jj),
      ii = ii,
      jj = jj,
      r = r_pc_nc,
      n = n
    ))
  
  # Run model
  pc_nc_fit <- stan(
    "models/2l-meta-analysis.stan",
    control = list(adapt_delta = 0.99, max_treedepth = 15),
    data = pc_nc_dlist,
    chains = n_cores,
    iter = n_warmup + n_iter/n_cores,
    warmup = n_warmup,
    seed = seeds[4]
  )  
  
  # Check
  stan_rhat(pc_nc_fit)
  stan_trace(
    pc_nc_fit, 
    pars = c("r_mean", "tau_ii", "tau_jj")
  )
  check_hmc_diagnostics(pc_nc_fit)
  print(
    pc_nc_fit, 
    pars = c("r_mean", "tau_ii", "tau_jj")
  )
  paste("Pr (r < 0) =", round(mean(as.data.frame(pc_nc_fit, "r_mean")$r_mean < 0), 3))

  
# Compile -----------------------------------------------------------------
  
  # Merge posterior draws from models
  post <- bind_rows(
      pi_fit %>% 
        spread_draws(r_pc_mean, r_nc_mean) %>% 
        mutate(y_var = "pi"),
     ca_fit %>% 
        spread_draws(r_pc_mean, r_nc_mean) %>% 
        mutate(y_var = "ca"),
      ps_fit %>% 
        spread_draws(r_pc_mean, r_nc_mean) %>% 
        mutate(y_var = "ps")
    ) %>% 
    pivot_longer(
      c(r_pc_mean, r_nc_mean),
      names_to = "x_var",
      names_pattern = "r_([a-z]*)_mean",
      values_to = "r_mean"
    ) %>% 
    bind_rows(
      pc_nc_fit %>% 
        spread_draws(r_mean) %>% 
        mutate(x_var = "pc", y_var = "nc")
    )
   
  # Add sample information
  post <- bind_rows(
      es %>% 
        group_by(y_var) %>% 
        summarize(I = n(), J = n_distinct(id), N = sum(n)),
      es %>% 
        distinct(id, sample, n) %>% 
        summarize(I = n(), J = n_distinct(id), N = sum(n)) %>% 
        mutate(y_var = "nc")
    ) %>% 
    left_join(post, ., by = "y_var") %>% 
    select(.chain:.draw, x_var, y_var, I, J, N, r_mean)


# Export ------------------------------------------------------------------
  
  # Export results (as .rds)
  write_rds(post, "results/results_analyses_with_negative_contact.rds")
    