rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------

  # Load packages
  library(tidyverse); library(tidybayes); library(ggtext); library(numform)

  # Link functions
  r_to_z <- function(r) 0.5 * log( (1 + r) / (1 - r) )
  z_to_r <- function(z) ( exp(2 * z) - 1 ) / ( exp(2 * z) + 1 )
  

# Prepare -----------------------------------------------------------------
  
  # Import results from moderator analyses
  results <- read_rds("results/results_categorical_moderator_analyses.rds")
  
  # Transform to long format
  results <- results %>% 
    select(y_var, moderator, r_kk) %>% 
    filter(moderator != "cultural_distance_to_usa") %>% 
    unnest(r_kk)
  
  # Name moderator and outcome variables
  results <- results %>% 
    mutate(
      y_name = recode_factor(
        y_var,
        "pi" = "Perceived injustice",
        "ca" = "Collective action",
        "ps" = "Policy support"
      ),
      moderator = recode_factor(
        moderator,
        "study_setting" = "Study setting",
        "study_design" = "Study design",
        "study_sample" = "Study\nsample",
        "study_intention" = "Study\nintention",
        "publication_status" = "Publication\nstatus",
        "ic_direct" = "Intergroup\ncontact",
        "pi_specific" = "Perceived\ninjustice",
        "pi_personal" = "Perceived\ninjustice "
      ),
      category = Hmisc::capitalize(category),
      category = recode(
        category,
        "Quasi-experimental (no random assignment)" = "Quasi-experimental",
        "Experimental (random assignment)" = "Experimental",
      ),
      category = factor(category, levels = c(
        "Observational, cross-sectional", 
        "Observational, longitudinal",
        "Quasi-experimental",
        "Experimental",
        "Short-term migration",
        "Long-term migration",
        "Slavery",
        "Colonization",
        "Religion",
        "Caste",
        "Sexuality",
        "Other",
        "Convenience sample",
        "Probability/representative sample",
        "Directly",
        "Indirectly",
        "Specific",
        "General",
        "Personal",
        "Group",
        "Both",
        "Published",
        "Unpublished",
        "Unpublished dissertation",
        "Yes",
        "No"
      ))
    )
  
  # Summarize posterior distributions
  d_4 <- results %>% 
    group_by(y_var, y_name, moderator, category, kk) %>% 
    median_qi(r_kk)
  
  # Add results from main analyses
  d_4 <- read_rds("results/results_preregistered_analyses.rds") %>% 
    filter(x_var == "ic") %>% 
    group_by(y_var) %>% 
    median_qi(r_mean, mu) %>% 
    select(-starts_with("mu")) %>% 
    left_join(
      d_4, ., by = c("y_var", ".width", ".point", ".interval")
    )

# Figure 4 ----------------------------------------------------------------

  # Visualize
  ggplot(d_4, aes(x = r_kk, y = fct_rev(category))) +
    geom_rect(
      aes(xmin = r_mean.lower, xmax = r_mean.upper),
      ymin = -Inf, ymax = Inf,
      fill = "grey92"
    ) +
    geom_hline(
      aes(yintercept = fct_rev(category)),
      colour = "grey92"
    ) +
    geom_pointrange(
      aes(xmin = .lower, xmax = .upper),
      size = 0.5,
      fatten = 1
    ) +
    geom_vline(
      xintercept = 0, 
      linetype = "dashed",
      size = 0.5
    ) +
    facet_grid(moderator ~ y_name, scales = "free_y", space = "free_y") +
    theme_classic(base_size = 10) +
    theme(
      legend.position = "none",
      strip.background = element_blank(),
      strip.text.y = element_text(vjust = 0, hjust = 0.5),
      axis.line = element_blank(),
      axis.text.y = element_markdown(colour = "black"),
      axis.text.x = element_text(colour = "black"),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_line(colour = "black"),
      panel.background = element_rect(colour = "black", fill = NA),
      panel.ontop = TRUE
    ) +
    labs(
      x = expression(italic(r)),
      y = NULL
    )


# Export ------------------------------------------------------------------
  
  # Export figure (as .pdf)
  ggsave(
    "figures/figure-4.pdf",
    width = 15.14, height = 15.14/3*4, units = "cm",
    device = cairo_pdf
  )
  
  # Export figure (as .png)
  ggsave(
    "figures/figure-4.png",
    width = 15.14, height = 15.14/3*4, units = "cm",
    dpi = 600
  )  
  