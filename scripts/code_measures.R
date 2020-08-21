rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------

  # Load packages
  library(tidyverse); library(irr)


# Import ------------------------------------------------------------------
  
  # Import results from survey
  dr <- read_csv(
    "records/results/coding-measures.csv",
    col_types = "ciiccccccc"
  )

  # Import .rds files (data)
  mi <- read_rds("data/mi.rds")
  

# Exclude -----------------------------------------------------------------

  # Remove excluded/failed studies
  dr <- dr %>% semi_join(mi, by = c("id", "sample"))
  

# Code measures -----------------------------------------------------------

  # Transform to long format
  dl <- dr %>% 
    pivot_longer(
      c(ic_direct, pi_specific, pi_personal),
      names_to = "item",
      values_to = "response"
    ) %>% 
    pivot_wider(
      names_from = coder, 
      values_from = response
    ) %>%
    filter(!is.na(`Coder 1`) | !is.na(`Coder 2`)) %>% 
    arrange(var, id, sample, variable, name, text, item)
    
  # Caclulate interrater agreement for predictor variable (directly/indirectly)
  dl %>% 
    filter(var == "ic", item == "ic_direct") %>% 
    with(., kappa2(ratings = cbind(`Coder 1`, `Coder 2`)))
  
  # Caclulate interrater agreement for outcome variable (specific/general/both)
  dl %>% 
    filter(var == "pi", item == "pi_specific") %>% 
    with(., kappa2(ratings = cbind(`Coder 1`, `Coder 2`)))
  
  # Caclulate interrater agreement for outcome variable (personal/group/both)
  dl %>% 
    filter(var == "pi", item == "pi_personal") %>% 
    with(., kappa2(ratings = cbind(`Coder 1`, `Coder 2`)))
  
  # Form consensus
  consensus <- c(
    "Directly", "Indirectly", "Indirectly", "Indirectly", "Indirectly",
    "Indirectly", "Directly", "Directly", "Directly", "Both", 
    "Personal", "Specific", "Personal", "Specific",  "Personal",
    "Specific", "Specific", "Group", "General", "General",
    "Specific", "Group", "General", "General", "Group",
    "Both", "Both", "Both", "Both", "Group",
    "General", "General", "Personal", "General", "General",
    "Both", "General", "General", "General", "General",
    "Group", "Group", "General", "General", "General", "Specific"
  )
    
  # Resolve disagreements
  mo_mi <- bind_rows(
      dl %>% 
        filter(`Coder 1` == `Coder 2`) %>% 
        mutate(response = unique(`Coder 1`, `Coder 2`)),
      dl %>%
        filter(`Coder 1` != `Coder 2`) %>% 
        mutate(response = consensus)
    ) %>% 
    select(-`Coder 1`, -`Coder 2`) %>% 
    pivot_wider(
      names_from = item,
      values_from = response
    ) %>% 
    arrange(id, sample, var)

  
# Export ------------------------------------------------------------------
  
  # Export as .csv files (data/csv/) 
  mo_mi %>% write_csv("data/csv/mo_mi.csv")
  
  # Export as .rds files (data)
  mo_mi %>% write_rds("data/mo_mi.rds")
    
  