rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------

  # Load packages
  library(tidyverse); library(ggtext); library(numform)


# Prepare -----------------------------------------------------------------

  # Import results from meta-bias analyses
  results <- read_rds("results/results_meta_biases.rds")
  

# Figure 7 ----------------------------------------------------------------

  # Visualize
  results %>% 
    mutate(
      estimate = if_else(method == "RMA", "unadjusted", "adjusted")
    ) %>% 
  ggplot(., aes(x = est)) +
    geom_rect(
      data = results %>% filter(method == "RMA") %>% select(-method),
      aes(xmin = l95, xmax = u95),
      ymin = -Inf, ymax = Inf,
      fill = "grey92"
    ) +
    geom_hline(
      aes(yintercept = fct_rev(method)),
      colour = "grey92"
    ) +
    geom_pointrange(
      aes(xmin = l95, xmax = u95, y = fct_rev(method), fill = estimate),
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
      aes(
        label = paste0(f_num(est, 2), ", [", f_num(l95, 2), ", ", f_num(u95, 2), "]"),
        y = fct_rev(method)
      ),
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
      ylim = c(0.5, 5.5),
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
    "figures/figure-7.pdf",
    width = 6.5, height = 6.5/5*2, units = "in",
    device = cairo_pdf
  )
  
  # Export figure (as .png)
  ggsave(
    "figures/figure-7.png",
    width = 6.5, height = 6.5/5*2, units = "in",
    dpi = 600
  )
