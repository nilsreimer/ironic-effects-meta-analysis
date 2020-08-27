rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------

  # Load packages
  library(tidyverse); library(ggtext); library(numform)


# Prepare -----------------------------------------------------------------

  # Import results from meta-bias analyses
  results <- read_rds("results/results_meta_biases.rds")
  

# Visualize ---------------------------------------------------------------

  # Make figure
  results %>% 
    mutate(
      estimate = if_else(method == "RMA", "unadjusted", "adjusted")
    ) %>% 
  ggplot(., aes(x = est, y = fct_rev(method))) +
    geom_rect(
      data = results %>% filter(method == "RMA"),
      aes(xmin = l95, xmax = u95),
      ymin = -Inf, ymax = Inf,
      fill = "grey92"
    ) +
    geom_hline(
      aes(yintercept = fct_rev(method)),
      colour = "grey92"
    ) +
    geom_pointrange(
      aes(xmin = l95, xmax = u95, fill = estimate),
      shape = "circle filled",
      fatten = 2,
      size = 0.5
    ) +
    geom_vline(
      xintercept = 0, 
      linetype = "dashed",
      size = 0.5
    ) +
    geom_richtext(
      aes(label = paste0(f_num(est, 2), ", [", f_num(l95, 2), ", ", f_num(u95, 2), "]")),
      x = 0.215,
      size = 8/.pt,
      hjust = 1, vjust = 0,
      label.padding = unit(c(0, 0, 0, 0), "lines"),
      label.margin = unit(c(0.2, 0.2, 0.2, 0.2), "lines"),
      label.r = unit(0, "lines"),
      label.colour = NA
    ) +
    scale_fill_manual(
      values = c(
        "adjusted" = "white",
        "unadjusted" = "black"
      )
    ) +
    coord_cartesian(
      xlim = c(-0.215, 0.215),
      ylim = c(0.5, 4.5),
      expand = FALSE
    ) +
    facet_grid(. ~ y_name) +
    theme_classic(base_size = 10) +
    theme(
      legend.position = "none",
      strip.background = element_blank(),
      axis.line = element_blank(),
      axis.text.y = element_markdown(colour = "black"),
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


# Export ------------------------------------------------------------------

  # Export figure (as .pdf)
  ggsave(
    "figures/figure-meta-biases.pdf",
    width = 15.14, height = 15.14/5*2, units = "cm",
    device = cairo_pdf
  )
  
  # Export figure (as .png)
  ggsave(
    "figures/figure-meta-biases.png",
    width = 15.14, height = 15.14/5*2, units = "cm",
    dpi = 600
  )
