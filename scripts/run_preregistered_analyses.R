rm(list = ls())

# Notes -------------------------------------------------------------------
  # Consider changing notation from i,j to j,k to accommodate multiple
  # effect sizes per sample.

  # 1494 should prioritise "major discrimination"

  # Add seeds for preregistered analyses

  # I should make 2381 and 1695 samples of the same study.

  # I should add the sample size required to find the range of effects.


# Library -----------------------------------------------------------------

  # Load packages
  library(tidyverse); library(rstan); library(tidybayes)

  # Stan options
  options(mc.cores = 8L)
  # Sys.setenv(LOCAL_CPPFLAGS = '-march=native')
  
  # Functions
  r_to_z <- function(r) 0.5 * log( (1 + r) / (1 - r) )
  z_to_r <- function(z) ( exp(2 * z) - 1 ) / ( exp(2 * z) + 1 )

  
# Prepare -----------------------------------------------------------------

  # Load (preregistered) model
  model <- stan_model("models/2l-meta-analysis.stan")
  
  # Import data
  dl <- read_rds("data/dl.rds")
  
  # Select outcomes
  dl <- dl %>% 
    filter(x_var == "ic", y_var %in% c("pi", "ca", "ps")) %>% 
    group_by(id, sample, y_var) %>% 
    top_n(1, -x_rank) %>%
    top_n(1, -y_rank) %>% 
    ungroup()

  # Average effect sizes for multiple (equivalent) outcomes
  es <- dl %>% 
    group_by(id, sample, x, y, x_var, y_var) %>% 
    summarise(
      n = round(mean(n, na.rm = TRUE)) %>% as.integer(), 
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
    with(.,list(
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
    chains = 8
  )
  
  # Check
  stan_rhat(pi_fit)
  stan_mcse(pi_fit)
  stan_ess(pi_fit)
  stan_trace(pi_fit, pars = c("r_mean", "tau_ii", "tau_jj"))
  check_hmc_diagnostics(pi_fit)
  print(pi_fit, pars = c("r_mean", "tau_ii", "tau_jj"))
  
  # Estimate range of expected effect sizes
  pi_fit %>% 
    spread_draws(mu, tau_jj) %>% 
    mutate(
      z = rnorm(n(), mu, tau_jj),
      r = z_to_r(z)
    ) %>% 
    # summarise(p = mean(r < 0)) %>% 
    median_qi(r, .width = c(.50, .80, .95)) %>%
    mutate_if(is.numeric, round, digits = 2)
  

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
    with(.,list(
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
    chains = 8
  )
  
  # Check
  stan_rhat(ca_fit)
  stan_mcse(ca_fit)
  stan_ess(ca_fit)
  stan_trace(ca_fit, pars = c("r_mean", "tau_ii", "tau_jj"))
  check_hmc_diagnostics(ca_fit)
  print(ca_fit, pars = c("r_mean", "tau_ii", "tau_jj"))
  
  # Estimate range of expected effect sizes
  ca_fit %>% 
    spread_draws(mu, tau_jj) %>% 
    mutate(
      z = rnorm(n(), mu, tau_jj),
      r = z_to_r(z)
    ) %>% 
    # summarise(p = mean(r < 0)) %>%
    median_qi(r, .width = c(.50, .80, .95)) %>%
    mutate_if(is.numeric, round, digits = 2)
  

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
    with(.,list(
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
    control = list(adapt_delta = 0.95),
    chains = 8
  )
  
  # Check
  stan_rhat(ps_fit)
  stan_mcse(ps_fit)
  stan_ess(ps_fit)
  stan_trace(ps_fit, pars = c("r_mean", "tau_ii", "tau_jj"))
  check_hmc_diagnostics(ps_fit)
  print(ps_fit, pars = c("r_mean", "tau_ii", "tau_jj"))
  
  # Estimate range of expected effect sizes
  ps_fit %>% 
    spread_draws(mu, tau_jj) %>% 
    mutate(
      z = rnorm(n(), mu, tau_jj),
      r = z_to_r(z)
    ) %>% 
    summarise(p = mean(r < 0)) %>%
    # median_qi(r, .width = c(.50, .80, .95)) %>%
    mutate_if(is.numeric, round, digits = 2)


# Sensitivity analysis (leave-one-out) ------------------------------------

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
      chains = 8
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
      chains = 8
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
      control = list(adapt_delta = 0.95),
      chains = 8,
      cores = 8
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
  
  # Estimate absolute error
  ae_loo <- bind_rows(
      pi_loo %>% mutate(outcome = "pi") %>% rename(r_loo = r_mean),
      ca_loo %>% mutate(outcome = "ca") %>% rename(r_loo = r_mean),
      ps_loo %>% mutate(outcome = "ps") %>% rename(r_loo = r_mean)
    ) %>% 
    left_join(
      bind_rows(
        pi_fit %>% spread_draws(r_mean) %>% mutate(outcome = "pi"),
        ca_fit %>% spread_draws(r_mean) %>% mutate(outcome = "ca"),
        ps_fit %>% spread_draws(r_mean) %>% mutate(outcome = "ps")
      )
    ) %>% 
    mutate(ae_loo = abs(r_mean - r_loo))
  
  # Calculate mean absolute error
  ae_loo %>% 
    group_by(.draw, outcome) %>% 
    summarise(mae = mean(ae_loo)) %>% 
    group_by(outcome) %>% 
    median_qi(mae) %>% 
    mutate_if(is.numeric, round, digits = 2)
  
  # Visualize
  ae_loo %>% 
    group_by(outcome, id) %>% 
    median_qi(r_mean, r_loo) %>% 
    group_by(outcome) %>% 
    mutate(id = row_number()) %>%
  ggplot(., aes(id, r_mean)) +
    geom_ribbon(
      aes(y = r_mean, ymin = r_mean.lower, ymax = r_mean.upper),
      fill = "red", alpha = 0.2
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_pointrange(
      aes(y = r_loo, ymin = r_loo.lower, ymax = r_loo.upper)
    ) +
    geom_line(colour = "red") +
    coord_flip() +
    facet_grid(outcome ~ ., scale = "free_y", space = "free_y")
