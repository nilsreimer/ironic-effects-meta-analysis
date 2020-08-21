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
  
  # Import results from extracting information about studies/measures
  si <- read_rds("data/si.rds")
  mi <- read_rds("data/mi.rds")


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
  

# Extract effect sizes ----------------------------------------------------

  # Extract effect sizes (r) from studies
  es <- results %>% 
    filter(str_detect(item, "r_[0-9]*_[0-9]")) %>% 
    extract(item, c("x", "y"), "r_([0-9]*)_([0-9]*)", convert = TRUE) %>% 
    mutate(across(c(x, y), numbers_to_names)) %>% 
    transmute(
      id, sample,
      metric = "r",
      es = as.numeric(response),
      x, y,
      note = NA_character_ 
    ) %>% 
    filter(!is.na(es)) %>% 
    arrange(id)
  
  # Extract effect sizes (other) from published studies
  source("records/results/extract_other_effect_sizes.R")

  # Extract effect sizes (other) from unpublished studies
  es <- read_csv(
        "records/results/effect-sizes-from-unpublished-studies.csv",
        col_types = "iicdcccciicc"
      ) %>% 
    bind_rows(es, .)
  
  
# Add missing information -------------------------------------------------
  
  # Add variable names to effect sizes (x)
  es <- es %>% 
    filter(is.na(x_name)) %>%
    select(-x_name) %>% 
    left_join(
      results %>% 
        filter(str_detect(item, "var_")) %>% 
        extract(item, "variable", "var_([0-9]*)_text", convert = TRUE) %>% 
        transmute(
          id, sample,
          x = numbers_to_names(variable),
          x_name = response
        ),
      by = c("id", "sample", "x")
    ) %>% 
    bind_rows(es %>% filter(!is.na(x_name))) %>% 
    mutate(x_name = if_else(is.na(x_name), name_variables(x, x_name), x_name))
  
  # Add variable names to effect sizes (y)
  es <- es %>% 
    filter(is.na(y_name)) %>%
    select(-y_name) %>% 
    left_join(
      results %>% 
        filter(str_detect(item, "var_")) %>% 
        extract(item, "variable", "var_([0-9]*)_text", convert = TRUE) %>% 
        transmute(
          id, sample,
          y = numbers_to_names(variable),
          y_name = response
        ),
      by = c("id", "sample", "y")
    ) %>% 
    bind_rows(es %>% filter(!is.na(y_name))) %>% 
    mutate(y_name = if_else(is.na(y_name), name_variables(y, y_name), y_name))
  
  # Add missing (reversed) correlations
  es <- anti_join(
    es %>% 
      filter(metric == "r", (is.na(t1) & is.na(t2)) | (t1 == t2)) %>% 
      rename(x = y, y = x, x_name = y_name, y_name = x_name) %>% 
      select(id:es, x, y, source:direction, x_name, y_name),
    es %>% 
      filter(metric == "r", (is.na(t1) & is.na(t2)) | (t1 == t2))
  ) %>% 
    bind_rows(es, .) %>% 
    arrange(id, sample, x, y)
  
  # Add missing (auto) correlations
  es <- anti_join(
    es %>% 
      filter(metric == "r", is.na(t1), is.na(t2)) %>% 
      distinct(id, sample, metric, x, source, note, n1, n2, t1, t2, direction, x_name) %>% 
      mutate(es = 1.0, y = x, y_name = x_name) %>% 
      select(id:metric, es, x, y, source:direction, x_name, y_name),
    es %>% 
      filter(metric == "r", is.na(t1), is.na(t2))
  ) %>% 
    bind_rows(es, .) %>% 
    arrange(id, sample, x, y)
  
  # Add source information
  es <- es %>% 
    mutate(
      source = case_when(
        !is.na(source) ~ source,
        between(id, 2380L, 2999L) ~ "unpublished",
        TRUE ~ "text"
      )
    )


# Correct -----------------------------------------------------------------
  
  # Remove study 2384 because it might overlap with 2392
  es <- es %>% filter(id != 2384L) 

  # Correct source
  es <- es %>% mutate(
    source = if_else(id == 1221L, "data", source),
    source = if_else(id == 2388L, "data", source)
  )
  
  # Correct variable names (in es)
  es <- es %>% 
    mutate(
      x = case_when(
        id ==  894L & x == "o1" ~ "ca",
        id == 1045L & x == "o1" ~ "pd",
        id == 1045L & x == "o2" ~ "pd",
        id == 1114L & x == "o1" ~ "ca",
        id == 1368L & x == "oc" ~ "cf",
        id == 1376L & x == "pi" ~ "ic",
        id == 1494L & sample >= 3L & x == "o1" ~ "cq",
        id == 1494L & sample >= 3L & x == "o2" ~ "cq",
        id == 1494L & sample >= 3L & x == "o3" ~ "pd",
        id == 1695L & x == "o1" ~ "ld",
        id == 1695L & x == "o2" ~ "ld",
        id == 1695L & x == "o3" ~ "ld",
        id == 1695L & x == "o4" ~ "ps",
        id == 1966L & x == "o1" ~ "cf",
        id == 1966L & x == "o2" ~ "cf",
        id == 1993L & x == "o1" ~ "in",
        id == 2309L & x == "o1" ~ "ca",
        id == 2381L & x == "o1" ~ "ps",
        id == 2381L & x == "o2" ~ "ps",
        id == 3205L & x == "o1" ~ "ps",
        id == 3205L & x == "o2" ~ "ps",
        TRUE ~ x
      ),
      y = case_when(
        id ==  894L & y == "o1" ~ "ca",
        id == 1045L & y == "o1" ~ "pd",
        id == 1045L & y == "o2" ~ "pd",
        id == 1114L & y == "o1" ~ "ca",
        id == 1368L & y == "oc" ~ "cf",
        id == 1376L & y == "pi" ~ "ic",
        id == 1494L & sample >= 3L & y == "o1" ~ "cq",
        id == 1494L & sample >= 3L & y == "o2" ~ "cq",
        id == 1494L & sample >= 3L & y == "o3" ~ "pd",
        id == 1695L & y == "o1" ~ "ld",
        id == 1695L & y == "o2" ~ "ld",
        id == 1695L & y == "o3" ~ "ld",
        id == 1695L & y == "o4" ~ "ps",
        id == 1966L & y == "o1" ~ "cf",
        id == 1966L & y == "o2" ~ "cf",
        id == 1993L & y == "o1" ~ "in",
        id == 2309L & y == "o1" ~ "ca",
        id == 2381L & y == "o1" ~ "ps",
        id == 2381L & y == "o2" ~ "ps",
        id == 3205L & y == "o1" ~ "ps",
        id == 3205L & y == "o2" ~ "ps",
        TRUE ~ y
      ),
      x_name = case_when(
        id == 1368L & x == "cf" ~ name_variables(x, NA_character_),
        id == 1376L & x == "ic" ~ name_variables(x, NA_character_),
        TRUE ~ x_name
      ),
      y_name = case_when(
        id == 1368L & y == "cf" ~ name_variables(y, NA_character_),
        id == 1376L & y == "ic" ~ name_variables(y, NA_character_),
        TRUE ~ y_name
      )
    )
  
  # Correct direction of effects
  es <- es %>% 
    mutate(
      es = case_when(
        id ==   45L & x == "pc" ~ -es,
        id ==  336L & x == "oa" ~ -es,
        id ==  823L & x == "pd" ~ -es,
        id ==  893L & x == "oc" ~ -es,
        id == 1163L & x == "oa" ~ -es,
        id == 1411L & x == "oa" ~ -es,
        id == 1695L & x == "ld" ~ -es,
        id == 1695L & x == "ps" ~ -es,
        TRUE ~ es
      ),
      es = case_when(
        id ==   45L & y == "pc" ~ -es,
        id ==  336L & y == "oa" ~ -es,
        id ==  823L & y == "pd" ~ -es,
        id ==  893L & y == "oc" ~ -es,
        id == 1163L & y == "oa" ~ -es,
        id == 1411L & y == "oa" ~ -es,
        id == 1695L & y == "ld" ~ -es,
        id == 1695L & y == "ps" ~ -es,
        TRUE ~ es
      ),
      note = case_when(
        id ==   45L & (x == "pc" | y == "pc") ~ "predictor reversed to reflect more positive contact",
        id ==  823L & (x == "pd" | y == "pd") ~ "outcome reversed to reflect more discrimination",
        id ==  830L & (x == "pd" | y == "pd") ~ "predictor reversed to reflect outgroup contact",
        id ==  893L & (x == "oc" | y == "oc") ~ "predictor reversed to reflect perceived outgroup contact",
        id == 1163L & (x == "oa" | y == "oa") ~ "outcome reversed to reflect more favourable attitudes",
        id == 1221L & (x == "pi" | y == "pi") ~ "outcome reversed to reflect more perceived prejudice",
        id == 1411L & (x == "oa" | y == "oa") ~ "outcome reversed to reflect more favourable attitudes",
        id == 1695L & (x == "ld" | y == "ld" | x == "ps" | y == "ps") ~ "outcome(s) reversed to reflect support of policies/illegitimacy of discrimination",
        TRUE ~ note
      )
    )
  
  # Correct names for time points
  es <- es %>% mutate(
    t1 = if_else(id == 2391L & t1 == 3L, 2L, t1),
    t2 = if_else(id == 2391L & t2 == 3L, 2L, t2)
  )
  
  # Change from unpublished to published
  es <- es %>% mutate(id = if_else(id == 2388L, 3054L, id)) 
  

# Export ------------------------------------------------------------------

  # Arrange for export
  es <- es %>% arrange(id, sample, x, y)
  
  # Export records
  write_csv(records, "records/records-extracted.csv")

  # Export as .csv files (data/csv/) 
  write_csv(es, "data/csv/es.csv")

  # Export as .rds files (data)
  write_rds(es, "data/es.rds")
