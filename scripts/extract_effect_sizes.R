rm(list = ls())

# Notes -------------------------------------------------------------------
  # Check source (for raw data)
  # Correct direction of effects
  # Correct variable names

# Library -----------------------------------------------------------------

  # Load packages
  library(tidyverse)

  # Load functions
  source("scripts/functions.R")


# Import ------------------------------------------------------------------

  # Import records
  records <- read_csv(
    "records/records-cleaned.csv",
    col_types = "icicccccccccccc"
  )
  
  # Import results from surveys
  results <- read_csv(
    "records/results/extracting-effect-sizes.csv", 
    col_types = "iciicc"
  )
  
  # Import status
  status <- read_csv(
    "records/results/recording-status.csv",
    col_types = "Diccc"
  )


# Exclude -----------------------------------------------------------------
  
  # Remove excluded/failed studies
  results <- semi_join(
    results,
    status %>% filter(status != "excluded", status != "failed"),
    by = "id"
  )
  
  # Remove excluded/failed records
  records <- semi_join(
    records,
    status %>% filter(status != "excluded", status != "failed"),
    by = "id"
  ) 


# Prepare -----------------------------------------------------------------

  # Compile study/sample information
  si <- results %>% 
    filter(item %in% c(
      "name", "n", "variables", 
      "r_reported", "b_reported", "other_reported", "longitudinal",
      "comments"
    )) %>% 
    distinct() %>% 
    pivot_wider(
      names_from = item,
      values_from = response
    ) %>% 
    mutate_at(vars(n, r_reported:longitudinal), as.integer) %>% 
    mutate(variables = numbers_to_names(variables)) %>% 
    arrange(id)
  
  # Compile measurement information
  mi <- results %>% 
    filter(str_detect(item, "items_[0-9]*")) %>% 
    extract(item, "variable", "items_([0-9]*)", convert = TRUE) %>% 
    transmute(
      id, coder, sample,
      variable = numbers_to_names(variable),
      text = response
    ) %>% 
  right_join(
    si %>% 
      select(id, sample, variables) %>% 
      separate_rows(variables),
    by = c("id", "sample", "variable" = "variables")
  ) %>% 
  left_join(
    results %>% 
      filter(str_detect(item, "var_")) %>% 
      extract(item, "variable", "var_([0-9]*)_text", convert = TRUE) %>% 
      transmute(
        id, sample,
        variable = numbers_to_names(variable),
        name = response
      ),
    by = c("id", "sample", "variable")
  ) %>% 
  mutate(name = name_variables(variable, name))
    
  # Extract effect sizes (published correlation coefficients)
  es <- results %>% 
    filter(str_detect(item, "r_[0-9]*_[0-9]")) %>% 
    extract(item, c("x", "y"), "r_([0-9]*)_([0-9]*)", convert = TRUE) %>% 
    mutate_at(vars(x, y), numbers_to_names) %>% 
    transmute(
      id, sample,
      metric = "r",
      es = as.numeric(response),
      x, y,
      source = "text",
      note = NA_character_ 
    ) %>% 
    filter(!is.na(es)) %>% 
    arrange(id)
  
  # Extract effect sizes (other)
  # source("records/results/extract_other_effect_sizes.R")
  # es <- bind_rows(es, es_unpublished)
  
  # Add effect sizes from unpublished effects (source = "unpublished")


# Correct -----------------------------------------------------------------

  
  
# Export ------------------------------------------------------------------

  # Export records
  write_csv(records, "records/records-extracted.csv")
  
  # Export as .csv (data/raw/) and as .rds (data)
