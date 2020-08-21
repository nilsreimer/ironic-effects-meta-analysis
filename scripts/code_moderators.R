rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------

  # Load packages
  library(tidyverse); library(countrycode); library(irr)


# Import ------------------------------------------------------------------
  
  # Import results from survey (published)
  moderators <- read_csv(
    "records/results/coding-moderators.csv", 
    col_types = "ciiccccccccc"
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
  
  # Import list of countries
  countries <- map_data("world") %>% 
    distinct(region) %>% 
    arrange(region) %>% 
    pull(region)

  
# Exclude -----------------------------------------------------------------
  
  # Remove excluded/failed studies (literature search)
  moderators <- semi_join(
    moderators,
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

# Code moderators (published) ---------------------------------------------
  
  # Rename variables (published)
  moderators <- moderators %>% 
    rename(
      country = q4,
      ingroup = q5,
      outgroup = q6,
      age = q7,
      study_sample = q8,
      study_sample_text = q8_4_text,
      study_design = q9,
      study_design_text = q9_6_text,
      study_intention = q10
    )
  
  # Caclulate interrater agreement for country
  moderators %>%
    select(id, sample, coder, country) %>% 
    pivot_wider(names_from = coder, values_from = country) %>%
    with(., kappa2(ratings = cbind(`Coder 1`, `Coder 2`)))
  
  # Caclulate interrater agreement for study design
  moderators %>%
    select(id, sample, coder, study_design) %>% 
    pivot_wider(names_from = coder, values_from = study_design) %>%
    with(., kappa2(ratings = cbind(`Coder 1`, `Coder 2`)))
  
  # Caclulate interrater agreement for study sample
  moderators %>%
    select(id, sample, coder, study_sample) %>% 
    pivot_wider(names_from = coder, values_from = study_sample) %>%
    with(., kappa2(ratings = cbind(`Coder 1`, `Coder 2`)))
  
  # Caclulate interrater agreement for age
  moderators %>%
    select(id, sample, coder, age) %>% 
    pivot_wider(names_from = coder, values_from = age) %>%
    with(., kappa2(ratings = cbind(`Coder 1`, `Coder 2`)))
  
  # Caclulate interrater agreement for study intention
  moderators %>%
    select(id, sample, coder, study_intention) %>% 
    pivot_wider(names_from = coder, values_from = study_intention) %>%
    with(., kappa2(ratings = cbind(`Coder 1`, `Coder 2`)))
  
  # Form consensus
  consensus <- tribble(
      ~id, ~sample,             ~item, ~response,
      19L,      1L,             "age", "Adults\n(≥ 18 years)",
     324L,      1L,             "age", "Adolescents\n(13 - 18 years)",
     324L,      2L,             "age", "Adolescents\n(13 - 18 years)",
     401L,      1L,             "age", "Adolescents\n(13 - 18 years),Adults\n(≥ 18 years)",
     703L,      1L,             "age", "Children\n(≤ 12 years)",
     711L,      1L,             "age", "Adults\n(≥ 18 years)",
     711L,      2L,             "age", "Adults\n(≥ 18 years)",
     830L,      2L,             "age", "Adolescents\n(13 - 18 years)",
    1283L,      1L,             "age", "Adults\n(≥ 18 years)",
    1395L,      1L,             "age", "Adults\n(≥ 18 years)",
    1494L,      1L,             "age", "Adolescents\n(13 - 18 years)",
    1494L,      2L,             "age", "Adolescents\n(13 - 18 years)",
    1762L,      1L,             "age", "Adolescents\n(13 - 18 years),Adults\n(≥ 18 years)",
    1807L,      1L,             "age", "Adults\n(≥ 18 years)",
    1949L,      1L,             "age", "Adolescents\n(13 - 18 years),Adults\n(≥ 18 years)",
    1966L,      1L,             "age", "Adolescents\n(13 - 18 years)",
    1966L,      2L,             "age", "Adolescents\n(13 - 18 years)",
    2023L,      1L,             "age", "Children\n(≤ 12 years)",
    2023L,      2L,             "age", "Children\n(≤ 12 years)",
    2333L,      1L,             "age", "Adults\n(≥ 18 years)",
     956L,      2L,         "country", "Germany,UK",
    2257L,      1L,         "country", "Turkey",
     894L,      2L,    "study_design", "observational, cross-sectional",
    2333L,      1L,    "study_design", "observational, cross-sectional",
    2341L,      1L,    "study_design", "observational, cross-sectional",
      45L,      1L, "study_intention", "Yes",
      45L,      2L, "study_intention", "Yes",
      45L,      3L, "study_intention", "Yes",
      91L,      1L, "study_intention", "Yes",
     244L,      1L, "study_intention", "Yes",
     703L,      1L, "study_intention", "Yes",
     893L,      1L, "study_intention", "Yes",
     933L,      1L, "study_intention", "Yes",
    1161L,      1L, "study_intention", "Yes",
    1163L,      1L, "study_intention", "Yes",
    1367L,      1L, "study_intention", "No",
    1407L,      1L, "study_intention", "Yes",
    1407L,      2L, "study_intention", "Yes",
    1467L,      1L, "study_intention", "Yes",
    1494L,      1L, "study_intention", "No",
    1494L,      2L, "study_intention", "No",
    1494L,      3L, "study_intention", "No",
    1494L,      4L, "study_intention", "No",
    1525L,      1L, "study_intention", "Yes",
    1576L,      1L, "study_intention", "Yes",
    1658L,      1L, "study_intention", "No",
    1658L,      2L, "study_intention", "No",
    1658L,      3L, "study_intention", "No",
    1807L,      1L, "study_intention", "Yes",
    1934L,      1L, "study_intention", "Yes",
    1934L,      2L, "study_intention", "Yes",
    1950L,      1L, "study_intention", "No",
    1950L,      2L, "study_intention", "No",
    2001L,      1L, "study_intention", "Yes",
    2023L,      1L, "study_intention", "No",
    2023L,      2L, "study_intention", "No",
    2257L,      1L, "study_intention", "No",
    2333L,      1L, "study_intention", "Yes",
    3196L,      1L, "study_intention", "Yes",
    3201L,      1L, "study_intention", "Yes",
      46L,      1L,    "study_sample", "probability or representative sample",
      91L,      1L,    "study_sample", "convenience sample, students (no random sampling)",
     317L,      1L,    "study_sample", "convenience sample, students (no random sampling)",
     317L,      2L,    "study_sample", "probability or representative sample",
     317L,      3L,    "study_sample", "probability or representative sample",
     609L,      1L,    "study_sample", "convenience sample, non-students (no random sampling)",
     829L,      1L,    "study_sample", "convenience sample, students (no random sampling)",
     830L,      1L,    "study_sample", "convenience sample, students (no random sampling)",
     830L,      2L,    "study_sample", "probability or representative sample",
     830L,      3L,    "study_sample", "probability or representative sample",
    1022L,      1L,    "study_sample", "convenience sample, students (no random sampling)",
    1161L,      1L,    "study_sample", "probability or representative sample",
    1367L,      1L,    "study_sample", "probability or representative sample",
    1368L,      1L,    "study_sample", "convenience sample, students (no random sampling)",
    1411L,      1L,    "study_sample", "probability or representative sample",
    1525L,      1L,    "study_sample", "probability or representative sample",
    1549L,      1L,    "study_sample", "probability or representative sample",
    1807L,      1L,    "study_sample", "probability or representative sample",
    1966L,      1L,    "study_sample", "convenience sample, non-students (no random sampling)",
    1966L,      2L,    "study_sample", "convenience sample, non-students (no random sampling)",
    1993L,      1L,    "study_sample", "convenience sample, non-students (no random sampling)",
    2001L,      1L,    "study_sample", "convenience sample, non-students (no random sampling)",
    2309L,      2L,    "study_sample", "convenience sample, non-students (no random sampling)",
    2341L,      1L,    "study_sample", "convenience sample, non-students (no random sampling)",
    3196L,      1L,    "study_sample", "convenience sample, students (no random sampling)"
  ) %>% semi_join(status %>% filter(status != "excluded", status != "failed"), by = "id")
  
  # Resolve disagreements
  moderators <- moderators %>% 
    select(-ingroup, -outgroup, -coder, -study_sample_text, -study_design_text) %>% 
    pivot_longer(
      -id:-sample,
      names_to = "item",
      values_to = "response",
      values_drop_na = TRUE
    ) %>% 
    anti_join(consensus, by = c("id", "sample", "item")) %>% 
    distinct() %>% 
    bind_rows(consensus) %>% 
    pivot_wider(
      names_from = item,
      values_from = response
    ) %>% 
    arrange(id, sample) %>% 
    left_join(
      moderators %>% 
        transmute(
          coder = recode(coder, "Coder 1" = "coder_1", "Coder 2" = "coder_2"), 
          id, sample, ingroup, outgroup
        ) %>% 
        pivot_wider(names_from = coder, values_from = c(ingroup, outgroup)),
      by = c("id", "sample")
    )
  

# Code moderators (unpublished/citing) ------------------------------------

  # Transform data (unpublished/citing)
  other <- bind_rows(unpublished, citing) %>% 
    select(id, sample, item, response) %>% 
    filter(str_detect(item, "age|country|ingroup|outgroup|study")) %>% 
    pivot_wider(
      names_from = item,
      values_from = response
    ) %>%
    transmute(
      id,
      sample,
      country,
      ingroup,
      outgroup,
      age = case_when(
        age_adolescents == 1L & age_adults == 1L ~ "Adolescents\n(13 - 18 years),Adults\n(≥ 18 years)",
        age_adolescents == 1L ~ "Adolescents\n(13 - 18 years)",
        age_adults == 1L ~ "Adults\n(≥ 18 years)",
        TRUE ~ NA_character_
      ),
      study_sample = recode(
        study_sample,
        "1" = "convenience sample, students (no random sampling)",
        "2" = "convenience sample, non-students (no random sampling)",
        "3" = "probability or representative sample",
        "4" = "other"
      ),
      study_sample_text,
      study_design = recode(
        study_design,
        "1" = "observational, cross-sectional",
        "2" = "observational, longitudinal",
        "3" = "quasi-experimental (no random assignment)",
        "4" = "experimental (random assignment)",
        "5" = "intervention",
        "6" = "other"
      ),
      study_design_text,
      study_intention = recode(
        study_intention,
        "1" = "Yes",
        "0" = "No"
      )
    )
  
  # Recode countries (unpublished/citing)
  other <- other %>% 
    separate_rows(country, sep = ",") %>% 
    mutate(country = countries[as.integer(country)]) %>% 
    group_by_at(vars(-country)) %>% 
    summarise(country = paste0(country, collapse = ",")) %>% 
    ungroup()
  

# Combine -----------------------------------------------------------------
  
  # Merge
  mo <- bind_rows(
    moderators,
    other %>% select(-ends_with("text"))
  )
  
  # Remove empty lines
  mo <- mo %>% filter(!is.na(id)) %>% arrange(id, sample)
  
  # Correct
  mo <- mo %>%
    mutate(
      country = case_when(
        id == 2392L & sample == 3L ~ "India",
        id %in% c(609L, 1395L, 3000L, 3050L) ~ "Hong Kong",
        TRUE ~ country
      )
    )

  # Change from unpublished to published
  mo <- mo %>% mutate(id = if_else(id == 2388L, 3054L, id)) 

  
# Code regions ------------------------------------------------------------

  # Code regions/continents
  mo <- mo %>% 
    distinct(id, sample, country) %>% 
    separate_rows(country, sep = ",") %>% 
    mutate(
      region = countrycode(
        sourcevar = country,
        origin = "country.name",
        destination = "region"
      ),
      region = case_when(
        country == "Macedonia" ~ "Europe & Central Asia",
        country == "Taiwan" ~ "East Asia & Pacific",
        TRUE ~ region
      ),
      continent = countrycode(
        sourcevar = country,
        origin = "country.name",
        destination = "continent"
      ),
      continent = case_when(
        country == "Kosovo" ~ "Europe",
        region == "Latin America & Caribbean" ~ "South America",
        region == "North America" ~ "North America",
        TRUE ~ continent
      )
    ) %>% 
    group_by(id, sample) %>% 
    summarise_at(vars(country, region, continent), ~paste(., collapse = ",")) %>% 
    mutate_at(vars(region, continent), ~if_else(str_detect(., ","), "Mixed", .)) %>% 
    ungroup() %>% 
    left_join(mo, ., by = c("id", "sample", "country"))


# Code groups -------------------------------------------------------------

  # Import results from survey
  groups <- read_csv(
    "records/results/coding-groups.csv", 
    col_types = "ciiccc"
  )
  
  # Remove excluded/failed studies
  groups <- semi_join(groups, mo, by = c("id", "sample"))
  
  # Caclulate interrater agreement for study setting
  groups %>% 
    pivot_wider(names_from = coder, values_from = study_setting) %>% 
    with(., kappa2(ratings = cbind(`Coder 1`, `Coder 2`)))
  
  # Form consensus
  consensus <- tribble(
      ~id, ~sample, ~study_setting,
     284L,      1L, "Colonization",
     325L,      1L, "Other",
     480L,      1L, "Long-term migration",
     893L,      1L, "Religion",
    1114L,      1L, "Long-term migration",
    1376L,      1L, "Religion",
    1807L,      1L, "Long-term migration",
    1949L,      1L, "Long-term migration",
    2383L,      3L, "Other",
    3000L,      1L, "Short-term migration",
    3005L,      1L, "Long-term migration",
    3054L,      4L, "Other",
    3054L,      6L, "Other",
    4000L,      1L, "Long-term migration",
    4001L,      1L, "Other"
  )
  
  # Resolve disagreements
  groups <- bind_rows(
    groups %>% 
      distinct(id, sample, ingroup, outgroup) %>% 
      semi_join(consensus, by = c("id", "sample")) %>% 
      left_join(consensus, by = c("id", "sample")),
    groups %>% 
      anti_join(consensus, by = c("id", "sample")) %>% 
      distinct(id, sample, ingroup, outgroup, study_setting)
  )
  
  # Differentiate long-term migration and slavery
  groups <- groups %>% 
    mutate(
      study_setting  = case_when(
        str_detect(ingroup, "Black American|African American|Afro Costa Ricans") ~ "Slavery",
        TRUE ~ study_setting
      )
    )
  
  # Merge with other moderators
  mo <- mo %>% 
    select(-starts_with("ingroup"), -starts_with("outgroup")) %>% 
    left_join(groups, by = c("id", "sample")) %>% 
    select(
      id, sample, country, region, continent, 
      ingroup, outgroup, study_setting,
      study_sample, age, study_design, study_intention,
    )
  
  
# Code cultural distance --------------------------------------------------

  # Import cultural distance to US (from http://culturaldistance.muth.io/)
  cdist <- read_csv("records/results/cultural-distance.csv")

  # Merge
  mo <- mo %>% 
    separate_rows(country, sep = ",") %>% 
    left_join(cdist, by = "country") %>% 
    group_by(id, sample) %>% 
    summarise(
      country = paste(country, collapse = ","),
      across(c(-country, -ends_with("_to_usa")), unique),
      cultural_distance_to_usa = paste(cultural_distance_to_usa, collapse = ","),
      political_distance_to_usa = paste(political_distance_to_usa, collapse = ","),
      political_participation_distance_to_usa = paste(political_participation_distance_to_usa, collapse = ","),
      group_membership_distance_to_usa = paste(group_membership_distance_to_usa, collapse = ",")
    ) %>% 
    ungroup()


# Code publication status -------------------------------------------------

  # Import source information
  published <- read_csv(
      "records/records-cleaned.csv",
      col_types = "icicccccccccccc"
    ) %>% 
    mutate(
      publication_status = if_else(
        str_detect(type, "Dissertation"),
        "unpublished dissertation",
        "published"
      )
    ) %>% 
    select(id, publication_status) %>% 
    filter(!is.na(publication_status))
  
  # Code publication status
  mo <- mo %>% 
    left_join(published, by = "id") %>% 
    mutate(
      publication_status = case_when(
        !is.na(publication_status) ~ publication_status,
        between(id, 2380L, 2999L) ~ "unpublished",
        id >= 4000L ~ "published",
        id == 2257L ~ "published"
      )
    )
  
  
# Corrections -------------------------------------------------------------
  
  # Data for samples 1–10 (2388/3054) is not differentiated in data.
  mo <- mo %>% 
    filter(id == 3054L) %>% 
    mutate(sample = if_else(sample == 11L, 11L, 1L)) %>% 
    pivot_longer(
      c(-id, -sample),
      names_to = "item",
      values_to = "response"
    ) %>% 
    distinct() %>% 
    group_by(id, sample, item) %>% 
    summarise(response = paste0(response, collapse = ", ")) %>% 
    ungroup() %>% 
    pivot_wider(
      names_from = item,
      values_from = response
    ) %>% 
    bind_rows(mo %>% filter(id != 3054L), .) %>% 
    arrange(id, sample)

  
# Export ------------------------------------------------------------------
  
  # Export as .csv files (data/csv/) 
  mo %>% write_csv("data/csv/mo.csv")
  
  # Export as .rds files (data)
  mo %>% write_rds("data/mo.rds")
  