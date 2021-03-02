rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------

  # Load packages
  library(tidyverse); library(tidybayes); library(ggrepel)

  # Link functions
  r_to_z <- function(r) 0.5 * log( (1 + r) / (1 - r) )
  z_to_r <- function(z) ( exp(2 * z) - 1 ) / ( exp(2 * z) + 1 )
  

# Prepare -----------------------------------------------------------------
  
  # Import results from preregistered analyses
  results <- read_rds("results/results_cultural_distance.rds")
  
  # Name predictor and outcome variables
  results <- results %>% 
    mutate(
      y_name = recode_factor(
        y_var,
        "pi" = "Perceived injustice",
        "ca" = "Collective action",
        "ps" = "Policy support"
      )
    )
  
  # Extract estimates
  r_fixed <- results %>% 
    select(-jj, -b_jj, -tau_jj, -country, -cultural_distance) %>% 
    distinct() %>% 
    crossing(cultural_distance = seq(0.0, 0.2, 0.001)) %>% 
    group_by(y_name, cultural_distance) %>% 
    mutate(
      r = z_to_r(mu + b_x*cultural_distance)
    ) %>% 
    median_qi(r)
  
  # Extract country-wise estimates
  r_country <- results %>% 
    group_by(y_name, country, cultural_distance) %>% 
    mutate(
      r = z_to_r(mu + b_x*cultural_distance + b_jj*tau_jj)
    ) %>% 
    median_qi(r)
  

# Figure 5 ----------------------------------------------------------------

  # Visualize
  ggplot(r_fixed, aes(x = cultural_distance, y = r)) +
    geom_ribbon(
      aes(ymin = .lower, ymax = .upper, fill = y_name),
      alpha = 0.4
    ) +
    geom_pointrange(
      data = r_country,
      aes(ymin = .lower, ymax = .upper),
      colour = "grey45",
      fatten = 1,
      size = 0.2
    ) +
    geom_text_repel(
      data = r_country,
      aes(label = country),
      size = 6/.pt, segment.size = 0.2,
      colour = "grey45"
    ) +
    geom_line(size = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", size = 0.5) +
    scale_x_continuous(breaks = seq(0, 1, 0.1)) +
    scale_y_continuous(breaks = seq(-1, 1, 0.1)) +
    scale_fill_manual(
      values = c(
        "Perceived injustice" = "#648FFF", 
        "Collective action" = "#DC267F", 
        "Policy support" = "#FFB000"
      )
    ) +
    facet_grid(. ~ y_name) +
    coord_cartesian(xlim = c(-0.01, 0.21), ylim = c(-0.425, 0.175), expand = FALSE) +
    theme_classic(base_size = 10) +
    theme(
      legend.position = "none",
      strip.background = element_blank(),
      axis.line = element_blank(),
      axis.text = element_text(colour = "black"),
      axis.ticks = element_line(colour = "black"),
      panel.background = element_rect(colour = "black", fill = NA),
      panel.ontop = TRUE
    ) +
    labs(
      x = "Cultural distance from the United States",
      y = expression(italic(r))
    )

  
# Export ------------------------------------------------------------------
  
  # Export figure (as .pdf)
  ggsave(
    "figures/figure-5.pdf",
    width = 15.14, height = 15.14/2, units = "cm",
    device = cairo_pdf
  )
  
  # Export figure (as .png)
  ggsave(
    "figures/figure-5.png",
    width = 15.14, height = 15.14/2, units = "cm",
    dpi = 600
  )  
  