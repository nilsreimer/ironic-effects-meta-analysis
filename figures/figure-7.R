rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------

  # Load packages
  library(tidyverse); library(ggtext); library(patchwork); library(numform)


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
        "pc" = "Positive contact",
        "nc" = "Negative contact",
        "og" = "Outgroup contact",
        "ig" = "Ingroup contact"
      ),
      y_name = recode_factor(
        y_var,
        "nc" = "Negative contact",
        "og" = "Outgroup contact",
        "pi" = "Perceived injustice",
        "ca" = "Collective action",
        "ps" = "Policy support",
      )
    )


# Figure 7a ---------------------------------------------------------------

  # Prepare
  d_7a <- results %>% 
    filter(x_var %in% c("pc", "nc"), y_var %in% c("pi", "ca", "ps")) %>% 
    mutate(
      text = paste0("*I* = ", I, ", *J* = ", J, ", *N* = ", f_comma(N))
    )
  
  # Visualize
  f_7a <- ggplot(d_7a, aes(x = r_mean)) +
    geom_density(
      aes(fill = x_name),
      colour = NA,
      adjust = 2,
      n = 1e4,
      alpha = 0.8
    ) +
    geom_density(
      aes(group = x_name),
      colour = "white",
      size = 0.2,
      adjust = 2, 
      n = 1e4
    ) +
    geom_richtext(
      data = d_7a %>% group_by(y_name) %>% summarise(text = unique(text)),
      aes(label = text),
      x = 0.315, y = 27,
      hjust = 1, vjust = 1,
      size = 10*0.8/.pt,
      label.colour = NA
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", size = 0.5) +
    scale_x_continuous(breaks = seq(-1, 1, 0.1)) +
    scale_fill_manual(
      values = c(
        "Positive contact" = "#648FFF",
        "Negative contact" = "#DC267F"
      )
    ) +
    facet_grid(y_name ~ .) +
    coord_cartesian(xlim = c(-0.315, 0.315), ylim = c(0, 27), expand = FALSE) +
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
      x = expression(italic(r[plain(mean)])),
      y = "Pr"
    )
  
  
# Figure 7b ---------------------------------------------------------------

  # Prepare
  d_7b <- results %>% 
    filter(x_var %in% c("ig", "og"), y_var %in% c("pi", "ca", "ps")) %>% 
    mutate(
      text = paste0("*I* = ", I, ", *J* = ", J, ", *N* = ", f_comma(N))
    )
  
  # Visualize
  f_7b <- ggplot(d_7b, aes(x = r_mean)) +
    geom_density(
      aes(fill = x_name),
      colour = NA,
      adjust = 2,
      n = 1e4,
      alpha = 0.8
    ) +
    geom_density(
      aes(group = x_name),
      colour = "white",
      size = 0.2,
      adjust = 2, 
      n = 1e4
    ) +
    geom_richtext(
      data = d_7b %>% group_by(y_name) %>% summarise(text = unique(text)),
      aes(label = text),
      x = 0.315, y = 15,
      hjust = 1, vjust = 1,
      size = 10*0.8/.pt,
      label.colour = NA
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", size = 0.5) +
    scale_x_continuous(breaks = seq(-1, 1, 0.1)) +
    scale_fill_manual(
      values = c(
        "Outgroup contact" = "#648FFF",
        "Ingroup contact" = "#FFB000"
      )
    ) +
    facet_grid(y_name ~ .) +
    coord_cartesian(xlim = c(-0.315, 0.315), ylim = c(0, 15), expand = FALSE) +
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
      x = expression(italic(r[plain(mean)])),
      y = "Pr"
    )


# Combine -----------------------------------------------------------------

  # Combine figures
  f_7a + f_7b + plot_layout(ncol = 1) + plot_annotation(tag_levels = "A")
  

# Export ------------------------------------------------------------------

  # Export figure (as .pdf)
  ggsave(
    "figures/figure-7.pdf",
    width = 15.14, height = 15.14/4*5, units = "cm",
    device = cairo_pdf
  )
  
  # Export figure (as .png)
  ggsave(
    "figures/figure-7.png",
    width = 15.14, height = 15.14/4*5, units = "cm",
    dpi = 600
  )
