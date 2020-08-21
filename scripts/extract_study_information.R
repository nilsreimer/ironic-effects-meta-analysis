rm(list = ls())

# Notes -------------------------------------------------------------------


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
  
  # Import results from survey
  results <- read_csv(
    "records/results/extracting-effect-sizes.csv", 
    col_types = "iciicc"
  )
  
  # Import results from survey (unpublished)
  unpublished <- read_csv(
    "records/results/unpublished-studies.csv", 
    col_types = "icciicc"
  )
  
  # Import results from survey (citing)
  citing <- read_csv(
    "records/results/citing-studies.csv", 
    col_types = "iiicc"
  )
  
  # Import status
  status <- read_csv(
    "records/results/recording-status.csv",
    col_types = "Dicc"
  )
  
  
# Exclude -----------------------------------------------------------------
  
  # Remove excluded/failed records
  records <- semi_join(
    records,
    status %>% filter(status != "excluded", status != "failed"),
    by = "id"
  ) 
  
  # Remove excluded/failed studies (literature search)
  results <- semi_join(
    results,
    status %>% filter(status != "excluded", status != "failed"),
    by = "id"
  )
  
  # Remove excluded/failed studies (citing studies)
  citing <- semi_join(
    citing,
    status %>% filter(status != "excluded", status != "failed"),
    by = "id"
  )
  
  # Remove excluded/failed studies (unpublished studies)
  unpublished <- semi_join(
    unpublished,
    status %>% filter(status != "excluded", status != "failed"),
    by = "id"
  )


# Prepare -----------------------------------------------------------------

  # Add unpublished studies
  results <- unpublished %>% 
    mutate(
      item = case_when(
        item == "ingroup" ~ "name",
        TRUE ~ item
      )
    ) %>% 
    filter(item %in% (distinct(results, item) %>% pull(item))) %>% 
    select(-reference, -preprint) %>% 
    bind_rows(results, .)
  
  # Add citing studies
  results <- citing %>% 
    mutate(
      item = case_when(
        item == "ingroup" ~ "name",
        TRUE ~ item
      )
    ) %>% 
    filter(item %in% (distinct(results, item) %>% pull(item))) %>% 
    bind_rows(results, .)
  

# Extract study information -----------------------------------------------
      
  # Compile study/sample information
  si <- results %>%
    filter(item %in% c(
      "name", "n", "variables", 
      "comments"
    )) %>% 
    distinct() %>% 
    pivot_wider(
      names_from = item,
      values_from = response
    ) %>% 
    mutate_at(vars(n), as.integer) %>% 
    mutate(variables = numbers_to_names(variables)) %>% 
    arrange(id)
  
  # Compile measurement information
  mi <- results %>% 
    filter(str_detect(item, "items_[0-9]*")) %>% 
    extract(item, "variable", "items_([0-9]*)", convert = FALSE) %>% 
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
  

# Corrections -------------------------------------------------------------

  # Data for samples 1–10 (2388/3054) is not differentiated in data.
  si <- si %>% 
    filter(id == 2388L) %>% 
    mutate(sample = if_else(sample == 11L, 11L, 1L)) %>% 
    group_by(id, coder, n_samples, sample, variables, comments) %>% 
    summarise(name = paste(name, collapse = ", "), n = sum(n)) %>% 
    bind_rows(
      si %>% filter(id != 2388L),
      .
    )
  mi <- mi %>% 
    filter(id == 2388L) %>% 
    mutate(sample = if_else(sample == 11L, 11L, 1L)) %>% 
    distinct(id, coder, sample, variable, text, name) %>% 
    bind_rows(
      mi %>% filter(id != 2388L),
      .
    )


# Correct -----------------------------------------------------------------
  
  # Remove study 2384 because it might overlap with 2392
  si <- si %>% filter(id != 2384L) 
  mi <- mi %>% filter(id != 2384L) 

  # Correct variable names (in mi)
  mi <- mi %>% 
    mutate(
      variable = case_when(
        id ==  894L & variable == "o1" ~ "ca",
        id == 1045L & variable == "o1" ~ "pd",
        id == 1045L & variable == "o2" ~ "pd",
        id == 1114L & variable == "o1" ~ "ca",
        id == 1368L & variable == "oc" ~ "cf",
        id == 1376L & variable == "pi" ~ "ic",
        id == 1494L & sample >= 3L & variable == "o1" ~ "cq",
        id == 1494L & sample >= 3L & variable == "o2" ~ "cq",
        id == 1494L & sample >= 3L & variable == "o3" ~ "pd",
        id == 1695L & variable == "o1" ~ "ld",
        id == 1695L & variable == "o2" ~ "ld",
        id == 1695L & variable == "o3" ~ "ld",
        id == 1695L & variable == "o4" ~ "ps",
        id == 1966L & variable == "o1" ~ "cf",
        id == 1966L & variable == "o2" ~ "cf",
        id == 1993L & variable == "o1" ~ "in",
        id == 2309L & variable == "o1" ~ "ca",
        id == 2381L & variable == "o1" ~ "ps",
        id == 2381L & variable == "o2" ~ "ps",
        id == 2391L & variable == "cq" ~ "pc",
        id == 3205L & variable == "o1" ~ "ps",
        id == 3205L & variable == "o2" ~ "ps",
        TRUE ~ variable
      ),
      name = case_when(
        id == 1368L & variable == "cf" ~ name_variables(variable, NA_character_),
        id == 1376L & variable == "ic" ~ name_variables(variable, NA_character_),
        id == 2391L & variable == "pc" ~ name_variables(variable, NA_character_),
        TRUE ~ name
      )
    )
  
  # Change from unpublished to published
  si <- si %>% mutate(id = if_else(id == 2388L, 3054L, id)) 
  mi <- mi %>% mutate(id = if_else(id == 2388L, 3054L, id)) 
  
  
# Export ------------------------------------------------------------------

  # Export as .csv files (data/csv/) 
  write_csv(si, "data/csv/si.csv")
  write_csv(mi, "data/csv/mi.csv")

  # Export as .rds files (data)
  write_rds(si, "data/si.rds")
  write_rds(mi, "data/mi.rds")
