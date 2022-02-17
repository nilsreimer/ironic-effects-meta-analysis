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
  rstan_options(auto_write = FALSE)
  
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
  model <- stan_model("models/1l-meta-analysis-categorical-moderators.stan")
  
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
          ii = row_number(),
          kk = as.integer(factor(category))
        )
      ),
      dlist = map(es, ~with(., list(
        I  = length(ii),
        K  = max(kk),
        ii = ii,
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
      ))
    )
  
  # Compile results
  if (exists("moderators")) {
    moderators <- bind_rows(moderators, results)
  } else {
    moderators <- results
  }
  

# Study design ------------------------------------------------------------

  # Load model
  model <- stan_model("models/1l-meta-analysis-categorical-moderators.stan")
  
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
          ii = row_number(),
          kk = as.integer(factor(category))
        )
      ),
      dlist = map(es, ~with(., list(
        I  = length(ii),
        K  = max(kk),
        ii = ii,
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
      ))
    )

  # Compile results
  if (exists("moderators")) {
    moderators <- bind_rows(moderators, results)
  } else {
    moderators <- results
  }


# Study sample ------------------------------------------------------------

  # Load model
  model <- stan_model("models/1l-meta-analysis-categorical-moderators.stan")
  
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
          ii = row_number(),
          kk = as.integer(factor(category))
        )
      ),
      dlist = map(es, ~with(., list(
        I  = length(ii),
        K  = max(kk),
        ii = ii,
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
      ))
    )
  
  # Compile results
  if (exists("moderators")) {
    moderators <- bind_rows(moderators, results)
  } else {
    moderators <- results
  }


# Age ---------------------------------------------------------------------

  # Load model
  model <- stan_model("models/1l-meta-analysis-categorical-moderators.stan")
  
  # Prepare data list
  results <- data %>% 
    mutate(
      moderator = "age",
      es = map(
        es,
        ~left_join(
          .,
          dl %>% distinct(id, sample, category = age),
          by = c("id", "sample")
        ) %>% mutate(
          category = case_when(
            str_detect(category, "Adults") & !str_detect(category, "Adolescents|Children") ~ "Adults",
            str_detect(category, "Adolescents") & !str_detect(category, "Children") ~ "Adolescents",
            str_detect(category, "Children") ~ "Children"
          ),
          ii = row_number(),
          kk = as.integer(factor(category))
        )
      ),
      dlist = map(es, ~with(., list(
        I  = length(ii),
        K  = max(kk),
        ii = ii,
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
        seed = 1060088
      ))
    )
  
  # Compile results
  if (exists("moderators")) {
    moderators <- bind_rows(moderators, results)
  } else {
    moderators <- results
  }



# Predictor variables (direct/indirect) -----------------------------------

  # Load model
  model <- stan_model("models/2l-meta-analysis-categorical-moderators.stan")

  # Prepare data list
  results <- data %>%
    mutate(
      moderator = "ic_direct",
      es = map(
        y_var,
        ~filter(dl, x_var == "ic", y_var == .) %>% 
          group_by(id, sample, ic_direct) %>% 
          top_n(1, -x_rank) %>% 
          top_n(1, -y_rank) %>% 
          group_by(id, sample, x, y, x_var, y_var, category = ic_direct) %>% 
          summarise(
            n = unique(n), 
            r = mean(r, na.rm = TRUE)
          ) %>% 
          ungroup() %>% 
          select(id, sample, n, r, everything()) %>% 
          mutate(
            ii = row_number(),
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
        control = list(adapt_delta = 0.99, max_treedepth = 12),
        chains = n_cores,
        iter = n_warmup + n_iter/n_cores,
        warmup = n_warmup,
        seed = 2106643
      ))
    )

  # Compile results
  if (exists("moderators")) {
    moderators <- bind_rows(moderators, results)
  } else {
    moderators <- results
  }


# Predictor variables (quality) -------------------------------------------

  # Load model
  model <- stan_model("models/2l-meta-analysis-categorical-moderators.stan")
  
  # Prepare data list
  results <- data %>%
    mutate(
      moderator = "ic_quality",
      es = map(
        y_var,
        ~filter(dl, x_var == "ic", y_var == .) %>% 
          group_by(id, sample, ic_direct) %>% 
          top_n(1, -x_rank) %>% 
          top_n(1, -y_rank) %>% 
          group_by(id, sample, x, y, x_var, y_var, category = ic_quality) %>% 
          summarise(
            n = unique(n), 
            r = mean(r, na.rm = TRUE)
          ) %>% 
          ungroup() %>% 
          select(id, sample, n, r, everything()) %>% 
          mutate(
            ii = row_number(),
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
        control = list(adapt_delta = 0.99, max_treedepth = 12),
        chains = n_cores,
        iter = n_warmup + n_iter/n_cores,
        warmup = n_warmup,
        seed = 2669059
      ))
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
          group_by(id, sample, pi_specific) %>% 
          top_n(1, -x_rank) %>% 
          top_n(1, -y_rank) %>% 
          group_by(id, sample, x, y, x_var, y_var, category = pi_specific) %>% 
          summarise(
            n = unique(n), 
            r = mean(r, na.rm = TRUE)
          ) %>% 
          ungroup() %>% 
          select(id, sample, n, r, everything()) %>% 
          mutate(
            ii = row_number(),
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
        control = list(adapt_delta = 0.99, max_treedepth = 12),
        chains = n_cores,
        iter = n_warmup + n_iter/n_cores,
        warmup = n_warmup,
        seed = 4531912
      ))
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
          group_by(id, sample, pi_personal) %>% 
          top_n(1, -x_rank) %>% 
          top_n(1, -y_rank) %>% 
          group_by(id, sample, x, y, x_var, y_var, category = pi_personal) %>% 
          summarise(
            n = unique(n), 
            r = mean(r, na.rm = TRUE)
          ) %>% 
          ungroup() %>% 
          select(id, sample, n, r, everything()) %>% 
          mutate(
            ii = row_number(),
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
        control = list(adapt_delta = 0.99, max_treedepth = 12),
        chains = n_cores,
        iter = n_warmup + n_iter/n_cores,
        warmup = n_warmup,
        seed = 1595735
      ))
    )

  # Compile results
  if (exists("moderators")) {
    moderators <- bind_rows(moderators, results)
  } else {
    moderators <- results
  }
  
  
# Publication status ------------------------------------------------------
  
  # Load model
  model <- stan_model("models/1l-meta-analysis-categorical-moderators.stan")
  
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
          category = recode(
            category,
            "published" = "published",
            "unpublished dissertation" = "unpublished",
            "unpublished" = "unpublished"
          ),
          ii = row_number(),
          kk = as.integer(factor(category))
        )
      ),
      dlist = map(es, ~with(., list(
        I  = length(ii),
        K  = max(kk),
        ii = ii,
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
      ))
    )

  # Compile results
  if (exists("moderators")) {
    moderators <- bind_rows(moderators, results)
  } else {
    moderators <- results
  }
  

# Study intention ---------------------------------------------------------

  # Load model
  model <- stan_model("models/1l-meta-analysis-categorical-moderators.stan")
  
  # Prepare data list
  results <- data %>% 
    mutate(
      moderator = "study_intention",
      es = map(
        es,
        ~left_join(
          .,
          dl %>% distinct(id, sample, category = study_intention),
          by = c("id", "sample")
        ) %>% mutate(
          ii = row_number(),
          kk = as.integer(factor(category))
        )
      ),
      dlist = map(es, ~with(., list(
        I  = length(ii),
        K  = max(kk),
        ii = ii,
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
        seed = 6414040
      ))
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
      ))
    )

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
    select(-es, -moderator, -dlist, -fit) %>% 
    unnest(post)


# Export ------------------------------------------------------------------

  # Export results (as .rds)
  moderators %>% 
    mutate(
      r_kk = map2(
        fit,
        es,
        ~spread_draws(.x, r_kk[kk]) %>% 
          left_join(
            distinct(.y, kk, category),
            by = "kk"
          )
      ),
      R2 = map(
        fit,
        ~spread_draws(., R2)
      )
    ) %>% 
    select(-dlist, -fit) %>% 
    write_rds("results/results_categorical_moderator_analyses.rds")
  
  # Export results for cultural distance (as .rds)
  results_cultural_distance %>% 
    write_rds("results/results_cultural_distance.rds")
