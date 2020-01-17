rm(list = ls())

# Notes -------------------------------------------------------------------
  # In longitudinal studies with more than two waves, I should decide on 
  # which intervals to include (e.g., t1 -> t3).


# Library -----------------------------------------------------------------

  # Load packages
  library(tidyverse)
  
  # Load functions
  source("scripts/functions.R")


# Import ------------------------------------------------------------------

  # Import .rds files (data)
  si <- read_rds("data/si.rds")
  mi <- read_rds("data/mi.rds")
  es <- read_rds("data/es.rds")


# Calculate effect sizes --------------------------------------------------

  # Convert to correlation coefficients (r)
  dl <- es %>% 
    mutate(
      r = case_when(
        metric == "r" ~ es,
        metric == "d" ~ d_to_r(es, n1, n2),
        metric == "eta2" ~ eta2_to_r(es, direction),
        metric == "or" ~ or_to_d(es) %>% d_to_r(., n1, n2),
        metric == "b" ~ b_to_r(es)
      )
    ) %>% 
    select(id:es, r, everything())
    
  # Add sample size (n)
  dl <- dl %>% 
    left_join(
      si %>% select(id, sample, n),
      by = c("id", "sample")
    ) %>% 
    mutate(
      n = case_when(
        !is.na(n1) & !is.na(n2) ~ n1 + n2,
        TRUE ~ n
      )
    ) %>% 
    select(id:es, n, r, everything())


# Calculate effect sizes for longitudinal studies -------------------------

  # Calculate cross-lagged effects (controlling for auto-regressive effects)
  dl <- left_join(
      dl %>% filter(metric == "r", x != y, x_name != y_name, t1 < t2) %>% 
        mutate(r_ab = r),
      dl %>% filter(metric == "r", x == y, x_name == y_name, t1 < t2) %>% 
        mutate(r_bc = r) %>% 
        select(id, sample, y, y_name, t1, t2, r_bc),
      by = c("id", "sample", "y", "y_name", "t1", "t2")
    ) %>%
    left_join(
      dl %>% 
        filter(metric == "r", x != y, x_name != y_name, t1 == t2) %>% 
        mutate(r_ac = r) %>% 
        select(id, sample, x, y, x_name, y_name, t1, r_ac),
      by = c("id", "sample", "x", "y", "x_name", "y_name", "t1")
    ) %>%
    mutate(r = partial_r(r_ab, r_ac, r_bc)) %>% 
    bind_rows(
      dl %>% filter(is.na(t1), is.na(t2)),
      .
    )
  
  # Exclude cross-lagged effects across two waves (e.g., t1 -> t3)
  dl <- dl %>% filter((is.na(t1) & is.na(t2)) | (t2 - t1) == 1L)
  

# Select/Arrange variables -------------------------------------------------------

  # Select variables
  dl <- dl %>% 
    select(
      id, sample, n, r, x, y, x_name, y_name, t1, t2, r_ab, r_bc, r_ac
    )
  

# Rank variables ----------------------------------------------------------

  # Rank variables
  dl <- dl %>% 
    mutate(
      x_var = case_when(
        x %in% c("oc", "cq", "pc", "cf", "ic") ~ "ic",
        x %in% c("pd", "gd", "ld", "rd", "pi") ~ "pi",
        x == "ps" ~ "ps",
        x == "ca" ~ "ca",
        TRUE ~ x
      ),
      y_var = case_when(
        y %in% c("oc", "cq", "pc", "cf", "ic") ~ "ic",
        y %in% c("pd", "gd", "ld", "rd", "pi") ~ "pi",
        y == "ps" ~ "ps",
        y == "ca" ~ "ca",
        TRUE ~ y
      ),
      x_rank = case_when(
        x == "cf" ~ 1L,
        x == "pc" ~ 2L,
        x == "ic" ~ 3L,
        x == "cq" ~ 4L,
        x == "oc" ~ 5L,
        x == "gd" ~ 1L,
        x %in% c("pi", "ld", "rd") ~ 2L,
        x == "pd" ~ 3L,
        x == "ps" ~ 1L,
        x == "ca" ~ 1L,
        TRUE ~ 1L
      ),
      y_rank = case_when(
        y == "cf" ~ 1L,
        y == "pc" ~ 2L,
        y == "ic" ~ 3L,
        y == "cq" ~ 4L,
        y == "oc" ~ 5L,
        y == "gd" ~ 1L,
        y %in% c("pi", "ld", "rd") ~ 2L,
        y == "pd" ~ 3L,
        y == "ps" ~ 1L,
        y == "ca" ~ 1L,
        TRUE ~ 1L
      )
    ) %>% 
    select(
      id:r, x, y, x_var, y_var, x_rank, y_rank, x_name, y_name, everything()
    )


# Export ------------------------------------------------------------------

  # Export as .csv files (data/csv/) 
  write_csv(dl, "data/csv/dl.csv")

  # Export as .rds files (data)
  write_rds(dl, "data/dl.rds")
