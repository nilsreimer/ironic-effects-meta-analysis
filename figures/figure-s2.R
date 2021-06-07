rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------

  # Load packages
  library(tidyverse); library(tidybayes); library(ggtext); 
  library(patchwork); library(numform)

  # Link functions
  r_to_z <- function(r) 0.5 * log( (1 + r) / (1 - r) )
  z_to_r <- function(z) ( exp(2 * z) - 1 ) / ( exp(2 * z) + 1 )
  

# Prepare -----------------------------------------------------------------
  
  # Import results from moderator analyses
  results <- read_rds("results/results_categorical_moderator_analyses.rds")
  
  # Transform to long format
  results <- results %>% 
    transmute(
      y_var, 
      moderator,
      draws = map2(
        r_kk,
        R2,
        ~left_join(.x, .y, by = c(".chain", ".iteration", ".draw"))
      )
    ) %>% 
    unnest(draws) %>% 
    left_join(
      results %>% 
        transmute(y_var, moderator, es = map(es, ~count(., kk, name = "I"))) %>% 
        unnest(es),
      by = c("y_var", "moderator", "kk")
    )
  
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
        "study_setting" = "Study Setting",
        "study_design" = "Study Design",
        "study_sample" = "Study Sample",
        "age" = "Age",
        "study_intention" = "Study Intention",
        "publication_status" = "Publication Status",
        "ic_direct" = "Predictor (Direct/Indirect)",
        "pi_specific" = "Outcome (Specific/General)",
        "pi_personal" = "Outcome (Personal/Group)"
      ),
      category = recode(
        category,
        "quasi-experimental (no random assignment)" = "quasi-experimental",
        "experimental (random assignment)" = "experimental",
        "convenience sample" = "convenience",
        "probability/representative sample" = "probability/representative",
        "Slavery" = "Post-Slavery",
        "Colonization" = "(Post-)Colonial"
      ),
      category = str_replace(category, " ", "\n"),
      category = str_replace(category, "\\/", "/\n"),
      category = Hmisc::capitalize(category),
      category = factor(category, levels = c(
        "Observational,\ncross-sectional", 
        "Observational,\nlongitudinal",
        "Quasi-experimental",
        "Experimental",
        "Short-term\nmigration",
        "Long-term\nmigration",
        "Post-Slavery",
        "(Post-)Colonial",
        "Religion",
        "Caste",
        "Sexuality",
        "Other",
        "Convenience",
        "Probability/\nrepresentative",
        "Adults",
        "Adolescents/\nChildren",
        "Directly",
        "Indirectly",
        "Specific",
        "General",
        "Personal",
        "Group",
        "Both",
        "Published",
        "Unpublished",
        "Yes",
        "No"
      ))
    )
  
# Figure s2 ---------------------------------------------------------------

  # Summarize posterior distributions
  d_s2 <- results %>% 
    filter(y_var == "ps") %>% 
    group_by(y_var, y_name, moderator, category, I, kk) %>% 
    median_qi(r_kk)
  
  # Add results from main analyses
  d_s2 <- read_rds("results/results_preregistered_analyses.rds") %>% 
    filter(x_var == "ic") %>% 
    group_by(y_var) %>% 
    median_qi(r_mean, mu) %>% 
    select(-starts_with("mu")) %>% 
    left_join(
      d_s2, ., by = c("y_var", ".width", ".point", ".interval")
    )
  
  # Add R2
  d_s2 <- results %>% 
    filter(y_var == "ps") %>% 
    group_by(y_var, y_name, moderator) %>% 
    distinct(.draw, R2) %>% 
    summarize(R2 = median(R2)) %>% 
    mutate(
      R2 = f_prop2percent(R2, digits = 0)
    ) %>% 
    left_join(d_s2, by = c("y_var", "y_name", "moderator"))
  
  # Visualize
  for (m in unique(d_s2$moderator)) {
    fig <- d_s2 %>% 
      filter(moderator == m) %>%
      ggplot(., aes(x = r_kk, y = fct_rev(category))) +
      geom_rect(
        aes(xmin = r_mean.lower, xmax = r_mean.upper),
        ymin = -Inf, ymax = Inf,
        fill = "grey92"
      ) +
      geom_hline(
        aes(yintercept = fct_rev(category)),
        colour = "grey92"
      ) +
      geom_linerange(
        aes(xmin = .lower, xmax = .upper),
        size = 0.5
      ) +
      geom_point(
        # aes(size = I)
      ) +
      geom_vline(
        xintercept = 0, 
        linetype = "dashed",
        size = 0.5
      ) +
      # scale_size(
      #   range = c(1, 4)
      # ) +
      coord_cartesian(xlim = c(-0.3, 0.1)) + 
      facet_grid(R2 ~ moderator) +
      theme_classic(base_size = 10) +
      theme(
        legend.position = "none",
        strip.background = element_blank(),
        strip.text.y = element_markdown(),
        axis.line = element_blank(),
        axis.text.y = element_text(colour = "black"),
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
    if (m == unique(d_s2$moderator)[1]) {
      f_s2 <- fig
    } else {
      f_s2 <- f_s2 + fig
    }
  }
  
  # Combine figures
  f_s2 + plot_layout(
    design = "
    AC
    AD
    AE
    BF
    BG
    ",
    guides = "collect"
  )


# Export ------------------------------------------------------------------
  
  # Export figure (as .pdf)
  ggsave(
    "figures/figure-s2.pdf",
    width = 6.5, height = 6.5, units = "in",
    device = cairo_pdf
  )
  
  # Export figure (as .png)
  ggsave(
    "figures/figure-s2.png",
    width = 6.5, height = 6.5, units = "in",
    dpi = 600
  )  
  