rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------
  library(tidyverse); library(revtools)


# Import ------------------------------------------------------------------
  
  # Import records
  dr <- read_csv("records/records-raw.csv") %>%
        mutate_at(vars(keywords), tolower)
  

# Remove duplicates -------------------------------------------------------
  
  # Find exact duplicates (doi)
  doi_matches <- find_duplicates(
    data = as.data.frame(dr),
    match_variable = "doi",
    match_function = "exact"
  )
  
  # Exclude exact duplicates (doi)
  dr <- extract_unique_references(dr, doi_matches)
  
  # Find exact matches (title)
  title_matches <- find_duplicates(
    data = as.data.frame(dr),
    match_variable = "title",
    group_variables = "year",
    match_function = "exact"
  )

  # Exclude exact duplicates (title)
  dr <- extract_unique_references(dr, title_matches)
  
  # Find fuzzy matches (title)
  title_matches <- find_duplicates(
    data = as.data.frame(dr),
    match_variable = "title",
    group_variables = "year",
    match_function = "fuzzdist", 
    method = "fuzz_m_ratio",
    threshold = 0.1
  )
  
  # Exclude fuzzy dublicates(title)
  dr <- extract_unique_references(dr, title_matches)
  

# Prepare -----------------------------------------------------------------

  # Assign unique ids / scramble order
  dr <- dr %>%
    as_tibble() %>%
    left_join(read_csv("records/records-ids.csv")) %>% 
    select(id, everything(), -n_duplicates) %>% 
    arrange(id)

  
# Export ------------------------------------------------------------------
  dr %>% write_csv("records/records-clean.csv")
