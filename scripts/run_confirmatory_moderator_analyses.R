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
  options(mc.cores = n_cores)
  rstan_options(auto_write = TRUE)
  
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
    group_by(id, sample, x, y, x_var, y_var) %>% 
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
  
  # Compile data as lists
  data <- tibble(
      y_var = c("pi", "ca", "ps"),
      es = list(
        es %>% filter(y_var == "pi"),
        es %>% filter(y_var == "ca"),
        es %>% filter(y_var == "ps")
      )
    )


# Study setting -----------------------------------------------------------

  # Load model
  model <- stan_model("models/2l-meta-analysis-categorical-moderators.stan")
  
  # Prepare data list
  results <- data %>% 
    mutate(
      moderator = "study_setting",
      es = map(
        es,
        ~left_join(
          .,
          dl %>% distinct(id, sample, category = study_setting),
          by = c("id", "sample")
        ) %>% filter(
          !str_detect(category, ",")
        ) %>% mutate(
          kk = as.integer(factor(category))
        )
      ),
      dlist = map(es, ~with(., list(
        I  = length(ii),
        J  = max(jj),
        K  = max(kk),
        ii = ii,
        jj = jj,
        kk = kk,
        r  = r,
        n  = n
      )))
    )
  
  # Run model
  results <- results %>% 
    mutate(
      fit = map(dlist, ~sampling(
        model,
        data = .,
        control = list(adapt_delta = 0.95),
        chains = n_cores,
        iter = n_warmup + n_iter/n_cores,
        warmup = n_warmup,
        seed = 4232087
      )),
      r_kk = map2(
        fit,
        es,
        ~spread_draws(.x, r_kk[kk]) %>% 
          left_join(
            distinct(.y, kk, category),
            by = "kk"
          )
      )
    )
  
  # Compile results
  if (exists("moderators")) {
    moderators <- bind_rows(moderators, results)
  } else {
    moderators <- results
  }
  

# Study design ------------------------------------------------------------

  # Load model
  model <- stan_model("models/2l-meta-analysis-categorical-moderators.stan")
  
  # Prepare data list
  results <- data %>% 
    mutate(
      moderator = "study_design",
      es = map(
        es,
        ~left_join(
          .,
          dl %>% distinct(id, sample, category = study_design),
          by = c("id", "sample")
        ) %>% mutate(
          kk = as.integer(factor(category))
        )
      ),
      dlist = map(es, ~with(., list(
        I  = length(ii),
        J  = max(jj),
        K  = max(kk),
        ii = ii,
        jj = jj,
        kk = kk,
        r  = r,
        n  = n
      )))
    )
  
  # Run model
  results <- results %>% 
    mutate(
      fit = map(dlist, ~sampling(
        model,
        data = .,
        control = list(adapt_delta = 0.99, max_treedepth = 12),
        chains = n_cores,
        iter = n_warmup + n_iter/n_cores,
        warmup = n_warmup,
        seed = 8493995
      )),
      r_kk = map2(
        fit,
        es,
        ~spread_draws(.x, r_kk[kk]) %>% 
          left_join(
            distinct(.y, kk, category),
            by = "kk"
          )
      )
    )
  
  # Compile results
  if (exists("moderators")) {
    moderators <- bind_rows(moderators, results)
  } else {
    moderators <- results
  }


# Study sample ------------------------------------------------------------

  # Load model
  model <- stan_model("models/2l-meta-analysis-categorical-moderators.stan")
  
  # Prepare data list
  results <- data %>% 
    mutate(
      moderator = "study_sample",
      es = map(
        es,
        ~left_join(
          .,
          dl %>% distinct(id, sample, category = study_sample),
          by = c("id", "sample")
        ) %>% mutate(
          category = recode(
            category,
            "convenience sample, non-students (no random sampling)" = "convenience sample",
            "convenience sample, students (no random sampling)" = "convenience sample",
            "probability or representative sample" = "probability/representative sample"
          ),
          kk = as.integer(factor(category))
        )
      ),
      dlist = map(es, ~with(., list(
        I  = length(ii),
        J  = max(jj),
        K  = max(kk),
        ii = ii,
        jj = jj,
        kk = kk,
        r  = r,
        n  = n
      )))
    )
  
  # Run model
  results <- results %>% 
    mutate(
      fit = map(dlist, ~sampling(
        model,
        data = .,
        control = list(adapt_delta = 0.95),
        chains = n_cores,
        iter = n_warmup + n_iter/n_cores,
        warmup = n_warmup,
        seed = 9708226
      )),
      r_kk = map2(
        fit,
        es,
        ~spread_draws(.x, r_kk[kk]) %>% 
          left_join(
            distinct(.y, kk, category),
            by = "kk"
          )
      )
    )
  
  # Compile results
  if (exists("moderators")) {
    moderators <- bind_rows(moderators, results)
  } else {
    moderators <- results
  }


# Predictor variables -----------------------------------------------------

  # Load model
  model <- stan_model("models/2l-meta-analysis-categorical-moderators.stan")
  
  # Prepare data list
  results <- data %>% 
    mutate(
      moderator = "ic_direct",
      es = map(
        y_var,
        ~filter(dl, x_var == "ic", y_var == .) %>% 
          group_by(id, sample, y_var) %>% 
          top_n(1, -y_rank) %>% 
          ungroup() %>% 
          group_by(id, sample, x, y, x_var, y_var, ic_direct) %>% 
          summarise(
            n = unique(n), 
            r = mean(r, na.rm = TRUE)
          ) %>% 
          ungroup() %>% 
          select(id, sample, n, r, everything(), category = ic_direct) %>% 
          mutate(
            ii = 1:n(),
            jj = as.integer(factor(id)),
            kk = as.integer(factor(category))
          ) %>% 
          group_by(jj) %>% 
          mutate(
            n_sample = n(),
            ii = if_else(n_sample == 1L, 0L, ii)
          ) %>% 
          ungroup() %>%
          mutate(
            ii = as.integer(factor(ii)) - 1
          ) %>% 
          select(-n_sample)
      ),
      dlist = map(es, ~with(., list(
        I  = length(ii),
        J  = max(jj),
        K  = max(kk),
        ii = ii,
        jj = jj,
        kk = kk,
        r  = r,
        n  = n
      )))
    )
  
  # Run model
  results <- results %>% 
    mutate(
      fit = map(dlist, ~sampling(
        model,
        data = .,
        control = list(adapt_delta = 0.95, max_treedepth = 12),
        chains = n_cores,
        iter = n_warmup + n_iter/n_cores,
        warmup = n_warmup,
        seed = 6898842
      )),
      r_kk = map2(
        fit,
        es,
        ~spread_draws(.x, r_kk[kk]) %>% 
          left_join(
            distinct(.y, kk, category),
            by = "kk"
          )
      )
    )
  
  # Compile results
  if (exists("moderators")) {
    moderators <- bind_rows(moderators, results)
  } else {
    moderators <- results
  }


# Outcome variables (specific/general) ------------------------------------

  # Load model
  model <- stan_model("models/2l-meta-analysis-categorical-moderators.stan")
  
  # Prepare data list
  results <- data %>% 
    filter(y_var == "pi") %>% 
    mutate(
      moderator = "pi_specific",
      es = map(
        y_var,
        ~filter(dl, x_var == "ic", y_var == .) %>% 
          group_by(id, sample, y_var) %>% 
          top_n(1, -y_rank) %>% 
          ungroup() %>% 
          group_by(id, sample, x, y, x_var, y_var, pi_specific) %>% 
          summarise(
            n = unique(n), 
            r = mean(r, na.rm = TRUE)
          ) %>% 
          ungroup() %>% 
          select(id, sample, n, r, everything(), category = pi_specific) %>% 
          mutate(
            ii = 1:n(),
            jj = as.integer(factor(id)),
            kk = as.integer(factor(category))
          ) %>% 
          group_by(jj) %>% 
          mutate(
            n_sample = n(),
            ii = if_else(n_sample == 1L, 0L, ii)
          ) %>% 
          ungroup() %>%
          mutate(
            ii = as.integer(factor(ii)) - 1
          ) %>% 
          select(-n_sample)
      ),
      dlist = map(es, ~with(., list(
        I  = length(ii),
        J  = max(jj),
        K  = max(kk),
        ii = ii,
        jj = jj,
        kk = kk,
        r  = r,
        n  = n
      )))
    )
  
  # Run model
  results <- results %>% 
    mutate(
      fit = map(dlist, ~sampling(
        model,
        data = .,
        control = list(adapt_delta = 0.95, max_treedepth = 12),
        chains = n_cores,
        iter = n_warmup + n_iter/n_cores,
        warmup = n_warmup,
        seed = 6898842
      )),
      r_kk = map2(
        fit,
        es,
        ~spread_draws(.x, r_kk[kk]) %>% 
          left_join(
            distinct(.y, kk, category),
            by = "kk"
          )
      )
    )
  
  # Compile results
  if (exists("moderators")) {
    moderators <- bind_rows(moderators, results)
  } else {
    moderators <- results
  }



# Outcome variables (personal/group) --------------------------------------

  # Load model
  model <- stan_model("models/2l-meta-analysis-categorical-moderators.stan")
  
  # Prepare data list
  results <- data %>% 
    filter(y_var == "pi") %>% 
    mutate(
      moderator = "pi_personal",
      es = map(
        y_var,
        ~filter(dl, x_var == "ic", y_var == .) %>% 
          group_by(id, sample, y_var) %>% 
          top_n(1, -y_rank) %>% 
          ungroup() %>% 
          group_by(id, sample, x, y, x_var, y_var, pi_personal) %>% 
          summarise(
            n = unique(n), 
            r = mean(r, na.rm = TRUE)
          ) %>% 
          ungroup() %>% 
          select(id, sample, n, r, everything(), category = pi_personal) %>% 
          mutate(
            ii = 1:n(),
            jj = as.integer(factor(id)),
            kk = as.integer(factor(category))
          ) %>% 
          group_by(jj) %>% 
          mutate(
            n_sample = n(),
            ii = if_else(n_sample == 1L, 0L, ii)
          ) %>% 
          ungroup() %>%
          mutate(
            ii = as.integer(factor(ii)) - 1
          ) %>% 
          select(-n_sample)
      ),
      dlist = map(es, ~with(., list(
        I  = length(ii),
        J  = max(jj),
        K  = max(kk),
        ii = ii,
        jj = jj,
        kk = kk,
        r  = r,
        n  = n
      )))
    )
  
  # Run model
  results <- results %>% 
    mutate(
      fit = map(dlist, ~sampling(
        model,
        data = .,
        control = list(adapt_delta = 0.95, max_treedepth = 12),
        chains = n_cores,
        iter = n_warmup + n_iter/n_cores,
        warmup = n_warmup,
        seed = 6898842
      )),
      r_kk = map2(
        fit,
        es,
        ~spread_draws(.x, r_kk[kk]) %>% 
          left_join(
            distinct(.y, kk, category),
            by = "kk"
          )
      )
    )
  
  # Compile results
  if (exists("moderators")) {
    moderators <- bind_rows(moderators, results)
  } else {
    moderators <- results
  }
  
  
# Publication status ------------------------------------------------------
  
  # Load model
  model <- stan_model("models/2l-meta-analysis-categorical-moderators.stan")
  
  # Prepare data list
  results <- data %>% 
    mutate(
      moderator = "publication_status",
      es = map(
        es,
        ~left_join(
          .,
          dl %>% distinct(id, sample, category = publication_status),
          by = c("id", "sample")
        ) %>% mutate(
          kk = recode(
            category,
            "published" = 1L,
            "unpublished dissertation" = 2L,
            "unpublished" = 3L
          )
        )
      ),
      dlist = map(es, ~with(., list(
        I  = length(ii),
        J  = max(jj),
        K  = max(kk),
        ii = ii,
        jj = jj,
        kk = kk,
        r  = r,
        n  = n
      )))
    )
  
  # Run model
  results <- results %>% 
    mutate(
      fit = map(dlist, ~sampling(
        model,
        data = .,
        control = list(adapt_delta = 0.99, max_treedepth = 12),
        chains = n_cores,
        iter = n_warmup + n_iter/n_cores,
        warmup = n_warmup,
        seed = 2522499
      )),
      r_kk = map2(
        fit,
        es,
        ~spread_draws(.x, r_kk[kk]) %>% 
          left_join(
            distinct(.y, kk, category),
            by = "kk"
          )
      )
    )

  # Compile results
  if (exists("moderators")) {
    moderators <- bind_rows(moderators, results)
  } else {
    moderators <- results
  }
  

# Cultural distance -------------------------------------------------------

  # Load model
  model <- stan_model("models/2l-meta-analysis-cultural-distance.stan")
  
  # Prepare data list
  results <- data %>% 
    mutate(
      moderator = "cultural_distance_to_usa",
      es = map(
        es,
        ~left_join(
          .,
          dl %>% distinct(id, sample, country, cultural_distance = cultural_distance_to_usa),
          by = c("id", "sample")
        ) %>% filter(
          !(cultural_distance == "NA"),
          !str_detect(country, ",")
        ) %>% mutate(
          cultural_distance = as.double(cultural_distance)
        ) %>% mutate(
          ii = 1:n(),
          jj = as.integer(factor(country))
        ) %>% group_by(jj) %>% mutate(
          n_sample = n(),
          ii = if_else(n_sample == 1L, 0L, ii)
        ) %>% ungroup() %>% mutate(
          ii = as.integer(factor(ii)) - 1
        ) %>% select(-n_sample)
      ),
      dlist = map(es, ~with(., list(
        I  = length(ii),
        J  = max(jj),
        ii = ii,
        jj = jj,
        r  = r,
        n  = n,
        x  = cultural_distance
      )))
    )
  
  # Run model
  results <- results %>% 
    mutate(
      fit = map(dlist, ~sampling(
        model,
        data = .,
        control = list(adapt_delta = 0.995, max_treedepth = 15),
        chains = n_cores,
        iter = n_warmup + n_iter/n_cores,
        warmup = n_warmup,
        seed = 7029818
      )),
      r_kk = map(
        fit,
        ~spread_draws(., mu, b_x) %>% 
          crossing(nesting(
            kk = 1:2,
            category = c("USA", "Cultural distance (50%tile)"),
            cultural_distance_to_usa = c(0, 0.1110656)
          )) %>% 
          mutate(
            r_kk = z_to_r(mu + b_x*cultural_distance_to_usa)
          )
      )
    )
  
  # Compile results
  if (exists("moderators")) {
    moderators <- bind_rows(moderators, results)
  } else {
    moderators <- results
  }
  
  # Extract country-level estimates
  results_cultural_distance <- results %>% 
    mutate(
      post = map2(
        fit,
        es,
        ~spread_draws(.x, mu, b_x, b_jj[jj], tau_jj) %>% 
          left_join(
            distinct(.y, jj, country, cultural_distance),
            by = "jj"
          )
      )
    ) %>% 
    select(-es, -moderator, -dlist, -fit, -r_kk) %>% 
    unnest(post)


# Export ------------------------------------------------------------------

  # Export results (as .rds)
  moderators %>% 
    select(-fit) %>% 
    write_rds("results/results_confirmatory_moderator_analyses.rds")
  
  # Export results for cultural distance (as .rds)
  results_cultural_distance %>% 
    write_rds("results/results_cultural_distance.rds")
