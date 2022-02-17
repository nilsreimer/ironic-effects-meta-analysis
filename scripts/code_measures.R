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
  
  # Import results from survey (additional moderator)
  dr_new <- read_csv(
    "records/results/coding-for-additional-analyses.csv",
    col_types = "cicccccc"
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
    "Indirectly", "Indirectly", "Directly", "Directly", "Directly", "Both", 
    "Personal", "Specific", "Personal", "Specific",  "Personal",
    "Specific", "Specific", "Group", "General", "General",
    "Specific", "Group", "General", "General", "Group",
    "Both", "Both", "Both", "Both", "Group",
    "General", "General", "Personal", "General", "General",
    "Both", "General", "General", "General", "General",
    "Group", "Group", "General", "General", "Specific", "General", "Specific"
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
  
  # Calculate interrater agreement for predictor variable (contact quality)
  dr_new %>% 
    pivot_wider(names_from = coder, values_from = ic_quality) %>% 
    with(., kappa2(ratings = cbind(`Coder 1`, `Coder 2`)))
  
  # Resolve disagreements
  dr_new <- dr_new %>% 
    mutate(
      ic_quality = case_when(
        id == 1045L & var == "cf" ~ "No",
        id == 1248L & var == "cq" ~ "Yes",
        id == 1386L & var == "cq" ~ "Yes",
        id == 1549L & var == "cf" ~ "No",
        id == 1966L & var == "cf" & name == "Best friend is immigrant (reversed)" ~ "Yes",
        id == 1966L & var == "cf" & name == "Cross-group friendship" ~ "Yes",
        id == 1993L & var == "pc" ~ "Yes",
        id == 1993L & var == "pc" ~ "Yes",
        id == 2257L & var == "ic" ~ "Yes",
        id == 2341L & var == "pc" ~ "Yes",
        id == 284L & var == "cf" ~ "Yes",
        id == 4002L & var == "pc" ~ "Yes",
        id == 4005L & var == "cq" ~ "No",
        id == 4005L & var == "ic" ~ "Yes",
        id == 4005L & var == "pc" ~ "Yes",
        TRUE ~ ic_quality
      )
    ) %>% 
    select(id, variable = var, name, ic_quality) %>%
    distinct()
  
  # Add contact quality as additional moderator
  mo_mi <- left_join(mo_mi, dr_new, by = c("id", "variable", "name")) %>% 
    select(id:text, ic_direct, ic_quality, pi_personal, pi_specific)

  
# Export ------------------------------------------------------------------
  
  # Export as .csv files (data/csv/) 
  mo_mi %>% write_csv("data/csv/mo_mi.csv")
  
  # Export as .rds files (data)
  mo_mi %>% write_rds("data/mo_mi.rds")
