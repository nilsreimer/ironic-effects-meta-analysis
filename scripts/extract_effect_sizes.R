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

  # Add unpublished study
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
  

# Extract -----------------------------------------------------------------

  # Extract effect sizes (r) from studies
  es <- results %>% 
    filter(str_detect(item, "r_[0-9]*_[0-9]")) %>% 
    extract(item, c("x", "y"), "r_([0-9]*)_([0-9]*)", convert = TRUE) %>% 
    mutate_at(vars(x, y), numbers_to_names) %>% 
    transmute(
      id, sample,
      metric = "r",
      es = as.numeric(response),
      x, y,
      source = if_else(id <= 2379L, "text", "unpublished"),
      note = NA_character_ 
    ) %>% 
    filter(!is.na(es)) %>% 
    arrange(id)
  
  # Extract effect sizes (other) from published studies
  source("records/results/extract_other_effect_sizes.R")

  # Extract effect sizes (other) from unpublished studies
  es <- read_csv(
      "records/results/effect-sizes-from-unpublished-studies.csv",
      col_types = "iicdccccii"
    ) %>% 
    bind_rows(es, .)
  
  # Add variable names to effect sizes
  es <- es %>% 
    left_join(mi %>% select(id, sample, x = variable, x_name = name)) %>% 
    left_join(mi %>% select(id, sample, y = variable, y_name = name))
    
  # Add missing (reversed) correlations
  es <- anti_join(
      es %>% 
        filter(metric == "r", is.na(t1), is.na(t2)) %>% 
        rename(x = y, y = x, x_name = y_name, y_name = x_name) %>% 
        select(id:es, x, y, source:direction, x_name, y_name),
      es %>% 
        filter(metric == "r", is.na(t1), is.na(t2))
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

# Correct -----------------------------------------------------------------
  
  # Correct source
  es <- es %>% mutate(source = if_else(id == 1221L, "data", source))
  
  # Correct variable names (in es)
  es <- es %>% 
    mutate(
      x = case_when(
        id ==  894L & x == "o1" ~ "ca",
        id == 1045L & x == "o1" ~ "pd",
        id == 1045L & x == "o2" ~ "pd",
        id == 1114L & x == "o1" ~ "ca",
        id == 1368L & x == "oc" ~ "cf",
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
        TRUE ~ x
      ),
      y = case_when(
        id ==  894L & y == "o1" ~ "ca",
        id == 1045L & y == "o1" ~ "pd",
        id == 1045L & y == "o2" ~ "pd",
        id == 1114L & y == "o1" ~ "ca",
        id == 1368L & y == "oc" ~ "cf",
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
        TRUE ~ y
      )
    )
  
  # Correct variable names (in mi)
  mi <- mi %>% 
    mutate(
      variable = case_when(
        id ==  894L & variable == "o1" ~ "ca",
        id == 1045L & variable == "o1" ~ "pd",
        id == 1045L & variable == "o2" ~ "pd",
        id == 1114L & variable == "o1" ~ "ca",
        id == 1368L & variable == "oc" ~ "cf",
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
        TRUE ~ variable
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
  
  # Check records
  # check_record <- function(i) {
  #   records %>% filter(id == i) %>% print()
  #   si %>% filter(id == i) %>% print(n = Inf)
  #   mi %>% filter(id == i) %>% print(n = Inf)
  #   es %>% filter(id == i) %>% print(n = Inf)
  #   mi %>% filter(id == i) %>% pull(text)
  # }
  
  
# Export ------------------------------------------------------------------

  # Export records
  write_csv(records, "records/records-extracted.csv")

  # Export as .csv files (data/csv/) 
  write_csv(si, "data/csv/si.csv")
  write_csv(mi, "data/csv/mi.csv")
  write_csv(es, "data/csv/es.csv")

  # Export as .rds files (data)
  write_rds(si, "data/si.rds")
  write_rds(mi, "data/mi.rds")
  write_rds(es, "data/es.rds")
  
  # Export for survey
  # si %>% 
  #   filter(id <= 2379L) %>% 
  #   distinct(id, sample, name) %>% 
  #   write_csv("../records/surveys/coding_moderators.csv")
