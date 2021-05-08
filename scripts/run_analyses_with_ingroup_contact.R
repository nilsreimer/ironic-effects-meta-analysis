rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------
  
  # Load packages
  library(tidyverse); library(rstan); library(tidybayes)
  
  # Stan options
  n_cores  <- 8
  n_iter   <- 8000
  n_warmup <- 1000
  seeds    <- c(6967674, 5768664, 7517370, 3223639)
  options(mc.cores = n_cores)
  rstan_options(auto_write = TRUE)
  
  # Functions
  source("scripts/functions.R")
  r_to_z <- function(r) 0.5 * log( (1 + r) / (1 - r) )
  z_to_r <- function(z) ( exp(2 * z) - 1 ) / ( exp(2 * z) + 1 )


# Prepare -----------------------------------------------------------------

  # Import data
  dl <- read_rds("data/dl.rds") %>% filter(!is.na(n), !is.na(r))
  
  # Select studies/samples
  dl <- dl %>% 
    filter(x == "in") %>% 
    distinct(id, sample) %>% 
    semi_join(dl, ., by = c("id", "sample"))
  
  # Select outgroup contact variable
  dl <- dl %>% 
    mutate(
      x = case_when(
        id ==  703L & x == "cf" ~ "og",
        id ==  733L & x == "ic" ~ "og",
        id ==  803L & x == "cq" ~ "og",
        id == 1020L & sample == 1L & x == "ic" ~ "og",
        id == 1020L & sample == 2L & x == "ic" ~ "og",
        id == 1042L & sample == 1L & x == "cq" ~ "og",
        id == 1042L & sample == 2L & x == "cq" ~ "og",
        id == 1695L & x == "cq" ~ "og",
        id == 1891L & x == "cf" ~ "og",
        id == 1914L & x == "in" ~ "og",
        id == 1993L & x == "cq" ~ "og",
        id == 2381L & x == "cf" ~ "og",
        id == 2382L & x == "cf" ~ "og",
        id == 2383L & x == "cf" ~ "og",
        id == 2396L & x == "cf" ~ "og",
        id == 2398L & x == "pc" ~ "og",
        TRUE ~ x
      ),
      y = case_when(
        id ==  703L & y == "cf" ~ "og",
        id ==  733L & y == "ic" ~ "og",
        id ==  803L & y == "cq" ~ "og",
        id == 1020L & sample == 1L & y == "ic" ~ "og",
        id == 1020L & sample == 2L & y == "ic" ~ "og",
        id == 1042L & sample == 1L & y == "cq" ~ "og",
        id == 1042L & sample == 2L & y == "cq" ~ "og",
        id == 1695L & y == "cq" ~ "og",
        id == 1891L & y == "cf" ~ "og",
        id == 1914L & y == "in" ~ "og",
        id == 1993L & y == "cq" ~ "og",
        id == 2381L & y == "cf" ~ "og",
        id == 2382L & y == "cf" ~ "og",
        id == 2383L & y == "cf" ~ "og",
        id == 2396L & y == "cf" ~ "og",
        id == 2398L & y == "pc" ~ "og",
        TRUE ~ y
      )
    ) %>% 
    filter(
      x_name != "Quality of Ingroup contact",
      y_name != "Quality of Ingroup contact"
    )
  
  # Prepare data
  es <- left_join(
    dl %>% 
      filter(x == "og", y_var %in% c("pi", "ca", "ps")) %>% 
      select(id, sample, n, y, y_var, y_name, r_og = r),
    dl %>% 
      filter(x == "in", y_var %in% c("pi", "ca", "ps")) %>% 
      select(id, sample, n, y, y_var, y_name, r_ig = r),
    by = c("id", "sample", "n", "y", "y_var", "y_name")
  ) %>% 
    left_join(
      dl %>% 
        filter(x == "og", y == "in") %>% 
        mutate(r = if_else(is.na(r_ac), r, r_ac)) %>% 
        select(id, sample, n, r_ig_og = r),
      by = c("id", "sample", "n")
    ) %>% 
    filter(!is.na(r_og), !is.na(r_ig), !is.na(r_ig_og))
  
  # Exclude irrelevant effect sizes
  es <- dl %>% 
    left_join(es) %>% 
    filter(
      !is.na(r_og), 
      !is.na(r_ig), 
      !is.na(r_ig_og),
      x %in% c("og", "in"),
      y_var %in% c("pi", "ca", "ps")
    ) %>% 
    select(id, sample, n, r, x:y_name, r_og:r_ig_og)
  
  # Calculate partial correlations
  es <- es %>%
    mutate(
      r = case_when(
        x == "og" ~ partial_r(r_og, r_ig_og, r_ig),
        x == "in" ~ partial_r(r_ig, r_ig_og, r_og),
        TRUE ~ NA_real_
      )
    ) %>% 
    mutate(x = if_else(x == "in", "ig", x))
  
  # Select outcomes
  es <- es %>% 
    group_by(id, sample, x, y_var) %>% 
    top_n(1, -y_rank) %>% 
    ungroup()
  
  # Transform to wide format
  es <- es %>% 
    select(id, sample, n, r, x, y, y_var, y_rank, y_name, r_ig_og) %>% 
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
  

# Estimate (perceived injustice) ------------------------------------------
  
  # Prepare data list
  pi_dlist <- es %>%
    filter(y_var == "pi") %>%
    transmute(
      ii = 1:n(),
      jj = as.integer(factor(id)),
      n, r_ig, r_og, r_ig_og
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
      r_ig = r_ig,
      r_og = r_og,
      r_ig_og = r_ig_og,
      n = n
    ))

  # Run model
  pi_fit <- stan(
    "models/2l-meta-analysis-with-ingroup-contact.stan",
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
    pars = c(
      "r_ig_mean", "tau_ig_ii", "tau_ig_jj",
      "r_og_mean", "tau_og_ii", "tau_og_jj",
      "r_ig_og_mean", "tau_ig_og_ii", "tau_ig_og_jj"
    ) 
  )
  check_hmc_diagnostics(pi_fit)
  print(
    pi_fit, 
    pars = c(
      "r_ig_mean", "tau_ig_ii", "tau_ig_jj",
      "r_og_mean", "tau_og_ii", "tau_og_jj",
      "r_ig_og_mean", "tau_ig_og_ii", "tau_ig_og_jj"
    )
  )
  paste("Pr (r > 0) =", round(mean(as.data.frame(pi_fit, "r_ig_mean")$r_ig_mean > 0), 3))
  paste("Pr (r < 0) =", round(mean(as.data.frame(pi_fit, "r_og_mean")$r_og_mean < 0), 3))
  
  
# Estimate (collective action) --------------------------------------------

  # Prepare data list
  ca_dlist <- es %>%
    filter(y_var == "ca") %>%
    transmute(
      ii = 1:n(),
      n, r_ig, r_og, r_ig_og
    ) %>%
    with(.,list(
      I = length(ii),
      ii = ii,
      r_ig = r_ig,
      r_og = r_og,
      r_ig_og = r_ig_og,
      n = n
    ))
  
  # Run model
  ca_fit <- stan(
    "models/1l-meta-analysis-with-ingroup-contact.stan",
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
    pars = c(
      "r_ig_mean", "tau_ig_ii", 
      "r_og_mean", "tau_og_ii",
      "r_ig_og_mean", "tau_ig_og_ii"
    ) 
  )
  check_hmc_diagnostics(ca_fit)
  print(
    ca_fit, 
    pars = c(
      "r_ig_mean", "tau_ig_ii",
      "r_og_mean", "tau_og_ii",
      "r_ig_og_mean", "tau_ig_og_ii"
    )
  )
  paste("Pr (r > 0) =", round(mean(as.data.frame(ca_fit, "r_ig_mean")$r_ig_mean > 0), 3))
  paste("Pr (r < 0) =", round(mean(as.data.frame(ca_fit, "r_og_mean")$r_og_mean < 0), 3))

  
# Estimate (policy support) -----------------------------------------------

  # Prepare data list
  ps_dlist <- es %>%
    filter(y_var == "ps") %>%
    transmute(
      ii = 1:n(),
      n, r_ig, r_og, r_ig_og
    ) %>%
    with(.,list(
      I = length(ii),
      ii = ii,
      r_ig = r_ig,
      r_og = r_og,
      r_ig_og = r_ig_og,
      n = n
    ))
  
  # Run model
  ps_fit <- stan(
    "models/1l-meta-analysis-with-ingroup-contact.stan",
    data = ps_dlist,
    control = list(adapt_delta = 0.9999, max_treedepth = 15),
    chains = n_cores,
    iter = n_warmup + n_iter/n_cores,
    warmup = n_warmup,
    seed = seeds[3]
  )  
  
  # Check
  stan_rhat(ps_fit)
  stan_trace(
    ps_fit, 
    pars = c(
      "r_ig_mean", "tau_ig_ii", 
      "r_og_mean", "tau_og_ii",
      "r_ig_og_mean", "tau_ig_og_ii"
    ) 
  )
  check_hmc_diagnostics(ps_fit)
  print(
    ps_fit, 
    pars = c(
      "r_ig_mean", "tau_ig_ii",
      "r_og_mean", "tau_og_ii",
      "r_ig_og_mean", "tau_ig_og_ii"
    )
  )
  paste("Pr (r > 0) =", round(mean(as.data.frame(ps_fit, "r_ig_mean")$r_ig_mean > 0), 3))
  paste("Pr (r < 0) =", round(mean(as.data.frame(ps_fit, "r_og_mean")$r_og_mean < 0), 3))
  

# Estimate association between predictors ---------------------------------
  
  # Prepare data list
  ig_og_dlist <- es %>%
    distinct(id, sample, n, r_ig_og) %>% 
    transmute(
      ii = 1:n(),
      jj = as.integer(factor(id)),
      n,
      r_ig_og
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
      r = r_ig_og,
      n = n
    ))
  
  # Run model
  ig_og_fit <- stan(
    "models/2l-meta-analysis.stan",
    control = list(adapt_delta = 0.99, max_treedepth = 15),
    data = ig_og_dlist,
    chains = n_cores,
    iter = n_warmup + n_iter/n_cores,
    warmup = n_warmup,
    seed = seeds[4]
  )  
  
  # Check
  stan_rhat(ig_og_fit)
  stan_trace(
    ig_og_fit, 
    pars = c("r_mean", "tau_ii", "tau_jj")
  )
  check_hmc_diagnostics(ig_og_fit)
  print(
    ig_og_fit, 
    pars = c("r_mean", "tau_ii", "tau_jj")
  )
  paste("Pr (r > 0) =", round(mean(as.data.frame(ig_og_fit, "r_mean")$r_mean > 0), 3))


# Compile -----------------------------------------------------------------
  
  # Merge posterior draws from models
  post <- bind_rows(
      pi_fit %>% 
        spread_draws(r_ig_mean, r_og_mean) %>% 
        mutate(y_var = "pi"),
      ca_fit %>% 
        spread_draws(r_ig_mean, r_og_mean) %>% 
        mutate(y_var = "ca"),
      ps_fit %>% 
        spread_draws(r_ig_mean, r_og_mean) %>% 
        mutate(y_var = "ps")
    ) %>% 
    pivot_longer(
      c(r_ig_mean, r_og_mean),
      names_to = "x_var",
      names_pattern = "r_([a-z]*)_mean",
      values_to = "r_mean"
    ) %>% 
    bind_rows(
      ig_og_fit %>% 
        spread_draws(r_mean) %>% 
        mutate(x_var = "ig", y_var = "og")
    )
  
  # Add sample information
  post <- bind_rows(
      es %>% 
        group_by(y_var) %>% 
        summarize(I = n(), J = n_distinct(id), N = sum(n)),
      es %>% 
        distinct(id, sample, n) %>% 
        summarize(I = n(), J = n_distinct(id), N = sum(n)) %>% 
        mutate(y_var = "og")
    ) %>% 
    left_join(post, ., by = "y_var") %>% 
    select(.chain:.draw, x_var, y_var, I, J, N, r_mean)
  
  
# Export ------------------------------------------------------------------
  
  # Export results (as .rds)
  write_rds(post, "results/results_analyses_with_ingroup_contact.rds")
  