rm(list = ls())

# Notes -------------------------------------------------------------------


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
  mo <- read_rds("data/mo.rds")
  mo_mi <- read_rds("data/mo_mi.rds")


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
  
  
# Combine -----------------------------------------------------------------

  # Add measure descriptions
  dl <- dl %>% 
    left_join(
      mi %>% 
        select(id, sample, x = variable, x_name = name, x_text = text) %>% 
        distinct(),
      by = c("id", "sample", "x", "x_name")
    ) %>% 
    left_join(
      mi %>% 
        select(id, sample, y = variable, y_name = name, y_text = text) %>% 
        distinct(),
      by = c("id", "sample", "y", "y_name")
    )
  
  # Add moderators (study/sample)
  dl <- dl %>% left_join(mo, by = c("id", "sample"))
  
  # Add moderators (measures)
  dl <- dl %>% 
    mutate(
      x_text = if_else(
        id == 4003L & x == "pc",
        mo_mi$text[mo_mi$id == 4003L & mo_mi$variable == "pc"],
        x_text
      )
    ) %>% 
    left_join(
      mo_mi %>% 
        filter(var == "ic") %>% 
        select(-starts_with("pi_")) %>% 
        rename_with(~paste0("x_", .), c("var", "name", "text")) %>% 
        rename(x = variable),
      by = c("id", "sample", "x", "x_var", "x_name", "x_text")
    ) %>% 
    left_join(
      mo_mi %>% 
        filter(var == "pi") %>% 
        select(-starts_with("ic_")) %>% 
        rename_with(~paste0("y_", .), c("var", "name", "text")) %>% 
        rename(y = variable),
      by = c("id", "sample", "y", "y_var", "y_name", "y_text")
    )


# Export ------------------------------------------------------------------

  # Select variables
  dl <- dl %>% 
    select(
      id, sample, metric, es, n1, n2, direction, n, r,
      x, y, x_var, y_var, x_rank, y_rank, x_name, y_name, x_text, y_text,
      source, note, t1, t2, r_ab, r_bc, r_ac,
      country, region, continent, ingroup, outgroup, study_setting, 
      study_design, study_sample, age, study_intention, publication_status,
      ic_direct, pi_specific, pi_personal,
      ends_with("to_usa")
    )
  
  # Remove missing effect/sample sizes
  dl <- dl %>% filter(!is.na(n), !is.na(r))
  
  # Arrange for export
  dl <- dl %>% arrange(id, sample, x, y)
  
  # Code missing moderators (for unpublished data)
  dl <- dl %>% 
    mutate(
      study_setting = case_when(
        !is.na(study_setting) ~ study_setting,
        id == 2395L & sample == 1L ~ "Colonization",
        id == 2395L & sample == 2L ~ "Long-term migration",
        id == 2396L & sample == 1L ~ "Other",
        id == 2396L & sample == 2L ~ "Sexuality",
        id == 2396L & sample == 3L ~ "Colonization",
        id == 2397L & sample == 1L ~ "Long-term migration"
      ),
      ic_direct = case_when(
        !is.na(ic_direct) ~ ic_direct,
        id == 2395L & sample == 1L ~ "Directly",
        id == 2395L & sample == 2L ~ "Directly",
        id == 2396L & sample == 1L ~ "Directly",
        id == 2396L & sample == 2L ~ "Directly",
        id == 2396L & sample == 3L ~ "Directly",
        id == 2397L & sample == 1L ~ "Directly"
      ),
      pi_specific = case_when(
        !is.na(pi_specific) ~ pi_specific,
        id == 2395L & sample == 1L & y == "gd" ~ "General",
        id == 2395L & sample == 1L & y == "pd" ~ "General",
        id == 2395L & sample == 1L & y == "rd" ~ "General",
        id == 2395L & sample == 2L & y == "gd" ~ "General",
        id == 2395L & sample == 2L & y == "pd" ~ "General",
        id == 2395L & sample == 2L & y == "rd" ~ "General",
        id == 2396L & sample == 1L & y == "pd" ~ "Specific",
        id == 2396L & sample == 2L & y == "pd" ~ "Specific",
        id == 2396L & sample == 3L & y == "pd" ~ "Specific",
        id == 2397L & sample == 1L & y == "rd" ~ "Specific"
      ),
      pi_personal = case_when(
        !is.na(pi_personal) ~ pi_personal,
        id == 2395L & sample == 1L & y == "gd" ~ "Group",
        id == 2395L & sample == 1L & y == "pd" ~ "Personal",
        id == 2395L & sample == 1L & y == "rd" ~ "Group",
        id == 2395L & sample == 2L & y == "gd" ~ "Group",
        id == 2395L & sample == 2L & y == "pd" ~ "Personal",
        id == 2395L & sample == 2L & y == "rd" ~ "Group",
        id == 2396L & sample == 1L & y == "pd" ~ "Personal",
        id == 2396L & sample == 2L & y == "pd" ~ "Personal",
        id == 2396L & sample == 3L & y == "pd" ~ "Personal",
        id == 2397L & sample == 1L & y == "rd" ~ "Group"
      )
    )
  
  # Export as .csv files (data/csv/) 
  write_csv(dl, "data/csv/dl.csv")

  # Export as .rds files (data)
  write_rds(dl, "data/dl.rds")
