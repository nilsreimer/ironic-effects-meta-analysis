rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------

  # Load packages
  library(tidyverse); library(metafor); library(weightr); library(puniform);
  library(ggtext); library(numform)

  # Link functions
  r_to_z <- function(r) 0.5 * log( (1 + r) / (1 - r) )
  z_to_r <- function(z) ( exp(2 * z) - 1 ) / ( exp(2 * z) + 1 )

  
# Prepare -----------------------------------------------------------------

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
    group_by(id, sample, x, y, x_var, y_var, publication_status, study_intention) %>% 
    summarise(
      n = unique(n), 
      r = mean(r, na.rm = TRUE)
    ) %>% 
    ungroup() %>% 
    select(id, sample, n, r, everything())
  
  # Transform effect sizes for analyses
  es <- es %>% 
    group_by(y_var) %>% 
    mutate(
      ii = 1:n(),
      yi = r_to_z(r),
      vi = 1 / (n - 3)
    ) %>% 
    ungroup()
  
  # Add predictor variable
  es <- es %>% 
    mutate(
      potential_bias = if_else(
        publication_status == "published" & study_intention == "Yes",
        1L,
        0L
      )
    )
  
  # Compile for analyses
  es <- tibble(
      y_var = c("pi", "ca", "ps"),
      es = list(
        es %>% filter(y_var == "pi"),
        es %>% filter(y_var == "ca"),
        es %>% filter(y_var == "ps")
      )
    ) %>% 
      mutate(
      es = map(es, ~transmute(
        ., 
        ii, 
        jj = as.integer(factor(id)), 
        yi, 
        vi,
        potential_bias
      ))
    )
  

# Estimate ----------------------------------------------------------------
  
  # Run analyses
  results <- es %>% 
    mutate(
      rma = map(es, ~rma.mv(
        yi = yi, 
        V = vi, 
        random = list(~ 1 | ii, ~ 1 | jj), 
        data = .
      )),
      pet = map(es, ~rma.mv(
        yi = yi, 
        V = vi, 
        random = list(~ 1 | ii, ~ 1 | jj),
        data = ., 
        mods = ~ sqrt(vi)
      )),
      peese = map(es, ~rma.mv(
        yi = yi,
        V = vi,
        random = list(~ 1 | ii, ~ 1 | jj),
        data = .,
        mods = ~ vi
      )),
      weightr = map(es, ~weightfunct(
        effect = .$yi, 
        v = .$vi
      )),
      punistar = map(es, ~puni_star(
        yi = .$yi,
        vi = .$vi, 
        side = "left"
      )),
      mod = map(es, ~rma.mv(
        yi = yi, 
        V = vi, 
        random = list(~ 1 | ii, ~ 1 | jj), 
        data = .,
        mods = ~ potential_bias
      ))
    )
  

# Extract -----------------------------------------------------------------

  # Extract estimates
  results <- results %>% 
    transmute(
      y_var,
      rma_est = map_dbl(rma, ~.$b[1,1]),
      rma_l95 = map_dbl(rma, ~.$ci.lb),
      rma_u95 = map_dbl(rma, ~.$ci.ub),
      pet_est = map_dbl(pet, ~.$b[1,1]),
      pet_l95 = map_dbl(pet, ~.$ci.lb[1]),
      pet_u95 = map_dbl(pet, ~.$ci.ub[1]),
      peese_est = map_dbl(peese, ~.$b[1,1]),
      peese_l95 = map_dbl(peese, ~.$ci.lb[1]),
      peese_u95 = map_dbl(peese, ~.$ci.ub[1]),
      petpeese_est = if_else(pet_u95 > 0, pet_est, peese_est),
      petpeese_l95 = if_else(pet_u95 > 0, pet_l95, peese_l95),
      petpeese_u95 = if_else(pet_u95 > 0, pet_u95, peese_u95),
      weightr_est = map_dbl(weightr, ~.$adj_est[2,1]),
      weightr_l95 = map_dbl(weightr, ~.$ci.lb_adj[2,1]),
      weightr_u95 = map_dbl(weightr, ~.$ci.ub_adj[2,1]),
      punistar_est = map_dbl(punistar, ~.$est),
      punistar_l95 = map_dbl(punistar, ~.$ci.lb),
      punistar_u95 = map_dbl(punistar, ~.$ci.ub),
      mod_est = map_dbl(mod, ~.$b[1,1]),
      mod_l95 = map_dbl(mod, ~.$ci.lb[1]),
      mod_u95 = map_dbl(mod, ~.$ci.ub[1])
    ) %>% 
    select(-starts_with("pet_"), -starts_with("peese_"))
  
  # Transform to long data
  results <- results %>% pivot_longer(
      c(-y_var),
      names_to = c("method", "parameter"),
      names_sep = "_",
      values_to = "estimate"
    ) %>% 
    pivot_wider(
      names_from = parameter,
      values_from = estimate
    ) %>% 
    mutate(
      across(c(est, l95, u95), z_to_r),
      method = recode_factor(
        method,
        "rma" = "RMA",
        "petpeese" = "PET-PEESE",
        "weightr" = "3PSM",
        "punistar" = "*p*-uniform\\*",
        "mod" = "Subgroup<br>Analysis"
      ),
      y_name = recode_factor(
        y_var,
        "pi" = "Perceived Injustice",
        "ca" = "Collective Action",
        "ps" = "Policy Support"
      )
    ) %>% 
    select(y_var, y_name, method, est, l95, u95)


# Export ------------------------------------------------------------------

  # Export results (as .rds)
  write_rds(results, "results/results_meta_biases.rds")
