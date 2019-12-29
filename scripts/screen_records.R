rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------

  # Load packages
  library(tidyverse); library(irr)


# Import ------------------------------------------------------------------

  # Import records
  records <- read_csv(
    "records/records-cleaned.csv",
    col_types = "icicccccccccccc"
  )

  # Import results from surveys
  ratings <- read_csv(
    "records/results/screening-records.csv", 
    col_types = "icci"
  )
  

# Prepare -----------------------------------------------------------------

  # Transform
  ratings <- ratings %>% mutate(decision = if_else(answer < 3L, 1L, 0L))


# Calculate ---------------------------------------------------------------

  # Caclulate interrater agreement for Trial 1 
  ratings %>% 
    filter(run == "Trial 1") %>% # (100 records rated by both coders)
    select(-answer) %>% 
    pivot_wider(names_from = coder, values_from = decision) %>% 
    with(., kappa2(ratings = cbind(`Coder 1`, `Coder 2`)))
  
  # Caclulate interrater agreement for Trial 2 
  ratings %>% 
    filter(run == "Trial 2") %>% # (100 records rated by both coders)
    select(-answer) %>% 
    pivot_wider(names_from = coder, values_from = decision) %>% 
    with(., kappa2(ratings = cbind(`Coder 1`, `Coder 2`)))
  
  # Caclulate interrater agreement for Trial 3 
  ratings %>% 
    filter(run == "Trial 3") %>% # (100 records rated by both coders)
    select(-answer) %>% 
    pivot_wider(names_from = coder, values_from = decision) %>% 
    with(., kappa2(ratings = cbind(`Coder 1`, `Coder 2`)))


# Exclude -----------------------------------------------------------------

  # Exclude duplicates
  ratings <- ratings %>%
    filter(
      !(id %in% c(
        641, 1112, 333, 1059, 996, 2243, 371, 1404, 1272, 1476, 493,
        1863, 121, 484, 1355, 939, 430, 1601, 1461, 1533, 885, 1338
      ))
    )

  # Include all records with interrater disagreement 
  ratings <- ratings %>%
    group_by(id) %>%
    mutate(
      include = if_else(sum(decision) > 0L, 1L, 0L)
    ) %>%
    ungroup()


# Export ------------------------------------------------------------------

  # Export screened records
  ratings %>% 
    filter(include == 1L) %>% 
    distinct(id, include) %>% 
    semi_join(records, ., by = "id") %>% 
    write_csv("records/records-screened.csv")
