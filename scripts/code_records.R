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
  
  # Remove missing/unpublished records
  records <- records %>% filter(!(id %in% c(
     505,  715,  719, 1057, 1070, 1156, 1180, 1242, 1336, 
    1344, 1359, 1541, 1675, 1734, 1875, 2013, 2087, 2194
  )))

  # Import results from surveys
  results <- read_csv(
    "records/results/coding-eligibility.csv", 
    col_types = "iciiiiiic"
  )

# Calculate ---------------------------------------------------------------
  
  # Caclulate interrater agreement for Q1
  results %>%
    select(id, coder, q1) %>% 
    pivot_wider(names_from = coder, values_from = q1) %>%
    with(., kappa2(ratings = cbind(`Coder 1`, `Coder 2`)))
  
  # Caclulate interrater agreement for Q2
  results %>%
    select(id, coder, q2) %>% 
    pivot_wider(names_from = coder, values_from = q2) %>%
    with(., kappa2(ratings = cbind(`Coder 1`, `Coder 2`)))
  
  # Caclulate interrater agreement for Q3
  results %>%
    select(id, coder, q3) %>% 
    pivot_wider(names_from = coder, values_from = q3) %>%
    with(., kappa2(ratings = cbind(`Coder 1`, `Coder 2`)))
  
  # Caclulate interrater agreement for Q4
  results %>%
    select(id, coder, q4) %>% 
    pivot_wider(names_from = coder, values_from = q4) %>%
    with(., kappa2(ratings = cbind(`Coder 1`, `Coder 2`)))

  # Caclulate interrater agreement for Q5
  results %>%
    select(id, coder, q5) %>% 
    pivot_wider(names_from = coder, values_from = q5) %>%
    with(., kappa2(ratings = cbind(`Coder 1`, `Coder 2`)))    

  # Caclulate interrater agreement for Q6
  results %>%
    select(id, coder, q6) %>% 
    pivot_wider(names_from = coder, values_from = q6) %>%
    with(., kappa2(ratings = cbind(`Coder 1`, `Coder 2`)))
  
  # Calculate interrater agreement for eligibility
  results %>% 
    transmute(
      id, 
      coder,
      include = as.integer(q1 == 1L & q2 == 1L & q3 == 1L & (q4 + q5 + q6) >= 1L)
    ) %>% 
    pivot_wider(names_from = coder, values_from = include) %>%
    with(., kappa2(ratings = cbind(`Coder 1`, `Coder 2`)))


# Exclude -----------------------------------------------------------------

  # Code eligibility/disagreement
  results <- results %>% 
    group_by(id) %>% 
    mutate(
      include = as.integer(q1 == 1L & q2 == 1L & q3 == 1L & (q4 + q5 + q6) >= 1L),
      agree = (include[1] == include[2])
    ) %>% 
    ungroup()
  
  # Resolve disagreements
  consensus <- tribble(
     ~id, ~include,
      45,       1L, 
     104,       0L, 
     363,       0L, 
     422,       0L, 
     684,       1L, 
     910,       1L, 
     920,       0L, 
    1170,       0L, 
    1217,       1L, 
    1221,       1L, 
    1376,       1L, 
    1658,       1L, 
    1750,       1L, 
    1812,       0L, 
    1850,       1L, 
    1891,       1L, 
    2001,       1L, 
    2023,       1L, 
    2291,       0L, 
    2295,       1L, 
    2371,       1L 
  )


# Export ------------------------------------------------------------------
  
  # Export eligible records
  bind_rows(
      results %>% filter(agree) %>% distinct(id, include),
      consensus
    ) %>% 
    filter(include == 1L) %>% 
    semi_join(records, ., by = "id") %>% 
    write_csv(., "records/records-coded.csv")
  