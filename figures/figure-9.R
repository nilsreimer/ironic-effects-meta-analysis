rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------

  # Load packages
  library(tidyverse)
  library(tidybayes)
  library(ggtext)
  library(patchwork)
  library(numform)


# Prepare -----------------------------------------------------------------
  
  # Import results from analyses with alternative predictors
  results <- bind_rows(
    read_rds("results/results_analyses_with_negative_contact.rds"),
    read_rds("results/results_analyses_with_ingroup_contact.rds")
  )

  # Name predictor and outcome variables
  results <- results %>% 
    mutate(
      x_name = recode_factor(
        x_var,
        "pc" = "Positive Contact",
        "nc" = "Negative Contact",
        "og" = "Outgroup Contact",
        "ig" = "Ingroup Contact"
      ),
      y_name = recode_factor(
        y_var,
        "nc" = "Negative Contact",
        "og" = "Outgroup Contact",
        "pi" = "Perceived Injustice",
        "ca" = "Collective Action",
        "ps" = "Policy Support",
      )
    )


# Figure 9a ---------------------------------------------------------------

  # Prepare
  d_9a <- results %>% 
    filter(x_var %in% c("pc", "nc"), y_var %in% c("pi", "ca", "ps")) %>% 
    mutate(
      text = paste0("*I* = ", I, ", *J* = ", J, ", *N* = ", f_comma(N))
    )
  
  # Visualize
  f_9a <- ggplot(d_9a, aes(x = r_mean)) +
    stat_halfeye(
      aes(fill = x_name),
      point_interval = NULL,
      adjust = 2,
      n = 1e4,
      alpha = 0.8
    ) +
    stat_halfeye(
      aes(group = x_name),
      point_interval = NULL,
      adjust = 2,
      n = 1e4,
      slab_colour = "white",
      slab_fill = NA,
      slab_size = 0.2
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", size = 0.5) +
    geom_richtext(
      data = d_9a %>% group_by(y_name) %>% summarise(text = unique(text)),
      aes(
        label = text, 
        hjust = if_else(y_name == "Perceived Injustice", 1, 0),
        x = if_else(y_name == "Perceived Injustice", 0.315, -0.315)
      ),
      y = 1,
      vjust = 1,
      size = 10*0.8/.pt,
      label.colour = NA
    ) +
    guides(
      fill = guide_legend(override.aes = list(shape = NA, colour = NA))
    ) +
    scale_x_continuous(breaks = seq(-1, 1, 0.1)) +
    scale_fill_manual(
      values = c(
        "Positive Contact" = "#648FFF",
        "Negative Contact" = "#DC267F"
      )
    ) +
    facet_grid(y_name ~ .) +
    coord_cartesian(xlim = c(-0.315, 0.315), ylim = c(0, 1), expand = FALSE) +
    theme_classic(base_size = 10) +
    theme(
      legend.position = c(0, 1),
      legend.justification = c(0, 1),
      legend.title = element_blank(),
      legend.background = element_rect(fill = NA),
      legend.key.size = unit(1, "line"),
      legend.margin = margin(0, 4, 0, 4),
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
      x = expression(italic(r[plain(partial)])),
      y = NULL
    )
  
  
# Figure 9b ---------------------------------------------------------------

  # Prepare
  d_9b <- results %>% 
    filter(x_var %in% c("ig", "og"), y_var %in% c("pi", "ca", "ps")) %>% 
    mutate(
      text = paste0("*I* = ", I, ", *J* = ", J, ", *N* = ", f_comma(N))
    )
  
  # Visualize
  f_9b <- ggplot(d_9b, aes(x = r_mean)) +
    stat_halfeye(
      aes(fill = x_name),
      point_interval = NULL,
      adjust = 2,
      n = 1e4,
      alpha = 0.8
    ) +
    stat_halfeye(
      aes(group = x_name),
      point_interval = NULL,
      adjust = 2,
      n = 1e4,
      slab_colour = "white",
      slab_fill = NA,
      slab_size = 0.2
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", size = 0.5) +
    geom_richtext(
      data = d_9b %>% group_by(y_name) %>% summarise(text = unique(text)),
      aes(
        label = text, 
        hjust = if_else(y_name == "Perceived Injustice", 1, 0),
        x = if_else(y_name == "Perceived Injustice", 0.315, -0.315)
      ),
      y = 1,
      vjust = 1,
      size = 10*0.8/.pt,
      label.colour = NA
    ) +
    guides(
      fill = guide_legend(override.aes = list(shape = NA, colour = NA))
    ) +
    scale_x_continuous(breaks = seq(-1, 1, 0.1)) +
    scale_fill_manual(
      values = c(
        "Outgroup Contact" = "#648FFF",
        "Ingroup Contact" = "#FFB000"
      )
    ) +
    facet_grid(y_name ~ .) +
    coord_cartesian(xlim = c(-0.315, 0.315), ylim = c(0, 1), expand = FALSE) +
    theme_classic(base_size = 10) +
    theme(
      legend.position = c(0, 1),
      legend.justification = c(0, 1),
      legend.title = element_blank(),
      legend.background = element_rect(fill = NA),
      legend.key.size = unit(1, "line"),
      legend.margin = margin(0, 4, 0, 4),
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
      x = expression(italic(r[plain(partial)])),
      y = NULL
    )


# Combine -----------------------------------------------------------------

  # Combine figures
  f_9a + f_9b + plot_layout(nrow = 1) + plot_annotation(tag_levels = "A")
  

# Export ------------------------------------------------------------------

  # Export figure (as .pdf)
  ggsave(
    "figures/figure-9.pdf",
    width = 6.5, height = 6.5/5*3, units = "in",
    device = cairo_pdf
  )
  
  # Export figure (as .png)
  ggsave(
    "figures/figure-9.png",
    width = 6.5, height = 6.5/5*3, units = "in",
    dpi = 600
  )
