# Rename variables
numbers_to_names <- function(numbers) {
  if (is.integer(numbers)) {
    names <- recode(
      numbers,
      "1"  = "oc",
      "2"  = "cq",
      "3"  = "pc",
      "4"  = "nc",
      "5"  = "cf",
      "6"  = "in",
      "7"  = "ic",
      "8"  = "pd",
      "9"  = "gd",
      "10" = "ld",
      "11" = "rd",
      "12" = "pi",
      "13" = "ps",
      "14" = "ca",
      "15" = "gi",
      "16" = "oa",
      "17" = "ge",
      "18" = "o1",
      "19" = "o2",
      "20" = "o3",
      "21" = "o4",
      .default = NA_character_
    )
  } else {
    names <- numbers %>%
      str_split(., ",") %>%
      map(
        ~recode(
          .,
          "1"  = "oc",
          "2"  = "cq",
          "3"  = "pc",
          "4"  = "nc",
          "5"  = "cf",
          "6"  = "in",
          "7"  = "ic",
          "8"  = "pd",
          "9"  = "gd",
          "10" = "ld",
          "11" = "rd",
          "12" = "pi",
          "13" = "ps",
          "14" = "ca",
          "15" = "gi",
          "16" = "oa",
          "17" = "ge",
          "18" = "o1",
          "19" = "o2",
          "20" = "o3",
          "21" = "o4",
          .default = NA_character_
        )
      ) %>%
      map(~str_c(., collapse = ",")) %>%
      unlist()
  }
  return(names)
}
name_variables <- function(variable, name) {
  case_when(
    variable == "oc" ~ "Opportunity for contact",
    variable == "cq" ~ "Contact quantity",
    variable == "pc" ~ "Contact quantity / Positive contact",
    variable == "nc" ~ "Negative contact",
    variable == "cf" ~ "Cross-group friendship",
    variable == "in" ~ "Ingroup contact",
    variable == "ic" ~ "Intergroup contact (other)",
    variable == "pd" ~ "Personal discrimination",
    variable == "gd" ~ "Group discrimination",
    variable == "ld" ~ "(Il-)legitimacy of discrimination",
    variable == "rd" ~ "Relative deprivation",
    variable == "pi" ~ "Perceived injustice (other)",
    variable == "ps" ~ "Policy support",
    variable == "ca" ~ "Collective action",
    variable == "gi" ~ "Group identificaiton",
    variable == "oa" ~ "Outgroup attitudes",
    variable == "ge" ~ "Group efficacy",
    variable == "o1" ~ name,
    variable == "o2" ~ name,
    variable == "o3" ~ name,
    variable == "o4" ~ name
  )
}

# Convert/compute effect sizes
b_to_r    <- function(beta) if_else(beta >= 0, beta + 0.05, beta)
d_to_r    <- function(d, n1, n2) d / sqrt(d^2 + ((n1 + n2)^2)/(n1*n2))
or_to_d   <- function(or) log(or)*sqrt(3)/pi 
eta2_to_r <- function(eta2, direction = c(-1, 1)) direction*sqrt(eta2) 
compute_d <- function(n1, m1, s1, n2, m2, s2) {
  (m1  - m2)/sqrt(((n1 - 1)*s1^2 + (n2 - 1)*s2^2)/(n1 + n2 - 2))
}
