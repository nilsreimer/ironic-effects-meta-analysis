rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------

  # Load packages
  library(tidyverse)
  library(tidybayes)
  library(ggtext)
  library(patchwork)
  library(numform)

  # Link functions
  r_to_z <- function(r) 0.5 * log( (1 + r) / (1 - r) )
  z_to_r <- function(z) ( exp(2 * z) - 1 ) / ( exp(2 * z) + 1 )
  
  # Set seed (for posterior predictive simulations)
  set.seed(9071118)
  

# Prepare -----------------------------------------------------------------
  
  # Import results from analyses
  pre_results <- read_rds("results/results_preregistered_analyses.rds")
  add_results <- read_rds("results/results_additional_analyses.rds")
  
  # Import study-wise estimates from analyses
  pre_r_pred_jj <- read_rds("results/r_pred_jj.rds")
  add_r_pred_jj <- read_rds("results/r_additional_analyses_pred_jj.rds")
  
  # Merge results
  results <- bind_rows(
    pre_results %>% mutate(analysis = "Preregistered"),
    add_results %>% mutate(analysis = "Not Preregistered")
  )
  
  # Merge study-wise estimates
  r_pred_jj <- bind_rows(
    pre_r_pred_jj %>% mutate(analysis = "Preregistered"),
    add_r_pred_jj %>% mutate(analysis = "Not Preregistered")
  )

  # Name predictor and outcome variables
  results <- results %>% 
    mutate(
      x_name = recode_factor(
        x_var,
        "ic" = "Intergroup Contact",
        "pi" = "Perceived Injustice",
        "ca" = "Collective Action",
        "ps" = "Policy Support"
      ),
      y_name = recode_factor(
        y_var,
        "pi" = "Perceived Injustice",
        "ca" = "Collective Action",
        "ps" = "Policy Support"
      )
    )
  
  # Name predictor and outcome variables
  r_pred_jj <- r_pred_jj %>% 
    mutate(
      x_name = recode_factor(
        x_var,
        "ic" = "Intergroup Contact",
      ),
      y_name = recode_factor(
        y_var,
        "pi" = "Perceived Injustice",
        "ca" = "Collective Action",
        "ps" = "Policy Support"
      )
    )
  

# Figure S3a --------------------------------------------------------------

  # Prepare
  d_s3a <- results %>% 
    filter(x_var == "ic") %>% 
    mutate(
      text = paste0("*I* = ", I, ", *J* = ", J, ", *N* = ", f_comma(N))
    )
  
  # Visualize
  f_s3a <- d_s3a %>% 
    filter(analysis == "Not Preregistered") %>% 
  ggplot(., aes(x = r_mean)) +
    stat_halfeye(
      aes(
        fill = y_name, 
        alpha = after_stat(x < 0)
      ),
      point_interval = NULL,
      adjust = 2,
      n = 1e4
    ) +
    stat_halfeye(
      data = d_s3a %>% filter(analysis == "Preregistered"),
      slab_colour = "black",
      slab_fill = NA,
      # slab_linetype = "dotted",
      slab_size = 0.5,
      point_interval = NULL,
      adjust = 2,
      n = 1e4
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", size = 0.5) +
    geom_richtext(
      data = d_s3a %>% 
        filter(analysis == "Not Preregistered") %>% 
        group_by(y_name) %>% 
        summarise(p = mean(r_mean < 0), r_mean = mean(r_mean)) %>% 
        mutate(p = if_else(p > 0.99, "\\>99%", paste0(round(p, 2)*100, "%"))),
      aes(label = p),
      y = 0,
      vjust = 0,
      size = 10*0.8/.pt,
      colour = "white", fill = NA, label.colour = NA
    ) +
    geom_richtext(
      data = d_s3a %>% 
        filter(analysis == "Not Preregistered") %>% 
        group_by(y_name) %>% 
        summarise(text = unique(text)),
      aes(label = text),
      x = 0.21, y = 1.0,
      hjust = 1, vjust = 1,
      size = 10*0.8/.pt,
      label.colour = NA
    ) + 
    scale_x_continuous(breaks = seq(-1, 1, 0.1)) +
    scale_fill_manual(
      values = c(
        "Perceived Injustice" = "#648FFF", 
        "Collective Action" = "#DC267F", 
        "Policy Support" = "#FFB000"
      )
    ) +
    scale_alpha_manual(
      values = c("TRUE" = 1.0, "FALSE" = 0.4)
    ) +
    facet_grid(y_name ~ .) +
    coord_cartesian(xlim = c(-0.21, 0.21), ylim = c(0.0, 1.0), expand = FALSE) +
    theme_classic(base_size = 10) +
    theme(
      legend.position = "none",
      strip.background = element_blank(),
      axis.line = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(colour = "black"),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_line(colour = "black"),
      panel.background = element_rect(colour = "black", fill = NA),
      panel.ontop = TRUE
    ) +
    labs(
      x = expression(italic(r[plain(mean)])),
      y = NULL
    )
  
  
# Figure S3b --------------------------------------------------------------

  # Prepare
  d_s3b <- results %>% 
    filter(x_var == "ic") %>% 
    mutate(
      text = paste0("*I* = ", I, ", *J* = ", J, ", *N* = ", f_comma(N)),
      r_pred = z_to_r(rnorm(n = n(), mean = mu, sd = tau_jj))
    )
  
  # Visualize
  f_s3b <- d_s3b %>% 
    filter(analysis == "Not Preregistered") %>% 
    ggplot(., aes(x = r_pred)) +
    stat_halfeye(
      aes(
        fill = y_name,
        alpha = after_stat(ggdist::cut_cdf_qi(cdf, c(0.8, 1.0)))
      ),
      point_interval = NULL,
      adjust = 2,
      n = 1e4
    ) +
    stat_halfeye(
      data = d_s3b %>% filter(analysis == "Preregistered"),
      slab_colour = "black",
      slab_fill = NA,
      # slab_linetype = "dotted",
      slab_size = 0.5,
      point_interval = NULL,
      adjust = 2,
      n = 1e4
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", size = 0.5) +
    geom_richtext(
      data = d_s3b %>% 
        filter(analysis == "Not Preregistered") %>% 
        group_by(y_name) %>% 
        summarise(r_pred = mean(r_pred)),
      label = "80%",
      y = 0,
      vjust = 0,
      size = 10*0.8/.pt,
      colour = "white", fill = NA, label.colour = NA
    ) + 
    geom_richtext(
      data = d_s3b %>% 
        filter(analysis == "Not Preregistered") %>% 
        distinct(y_name, text),
      aes(label = text),
      x = 0.42, y = 1.0,
      hjust = 1, vjust = 1,
      size = 10*0.8/.pt,
      label.colour = NA
    ) +
    geom_point(
      data = r_pred_jj %>%
        filter(analysis == "Not Preregistered") %>%
        group_by(x_name, y_name, jj) %>%
        summarize(r_pred = median(r_pred)),
      y = 0,
      shape = "|", size = 3
    ) +
    facet_grid(y_name ~ .) +
    scale_x_continuous(
      limits = c(-0.42, 0.42),
      breaks = seq(-1, 1, 0.2)
    ) +
    scale_fill_manual(
      values = c(
        "Perceived Injustice" = "#648FFF", 
        "Collective Action" = "#DC267F", 
        "Policy Support" = "#FFB000"
      )
    ) +
    scale_alpha_manual(
      values = c("0.8" = 1.0, "1" = 0.4)
    ) +
    facet_grid(y_name ~ .) +
    coord_cartesian(
      ylim = c(0.0, 1.0), 
      expand = FALSE
    ) +
    theme_classic(base_size = 10) +
    theme(
      legend.position = "none",
      strip.background = element_blank(),
      axis.line = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(colour = "black"),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_line(colour = "black"),
      panel.background = element_rect(colour = "black", fill = NA),
      panel.ontop = TRUE
    ) +
    labs(
      x = expression(italic(r[plain(predicted)])),
      y = NULL
    )


# Combine -----------------------------------------------------------------

  # Combine figures
  f_s3a + f_s3b + plot_layout(nrow = 1) + plot_annotation(tag_levels = "A")
  

# Export ------------------------------------------------------------------

  # Export figure (as .pdf)
  ggsave(
    "figures/figure-s3.pdf",
    width = 6.5, height = 6.5/5*3, units = "in",
    device = cairo_pdf
  )
  
  # Export figure (as .png)
  ggsave(
    "figures/figure-s3.png",
    width = 6.5, height = 6.5/5*3, units = "in",
    dpi = 600
  )
