rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------

  # Load packages
  library(tidyverse); library(ggtext); library(patchwork); library(numform)

  # Link functions
  r_to_z <- function(r) 0.5 * log( (1 + r) / (1 - r) )
  z_to_r <- function(z) ( exp(2 * z) - 1 ) / ( exp(2 * z) + 1 )
  

# Prepare -----------------------------------------------------------------
  
  # Import results from preregistered analyses
  results <- read_rds("results/results_preregistered_analyses.rds")

  # Name predictor and outcome variables
  results <- results %>% 
    mutate(
      x_name = recode_factor(
        x_var,
        "ic" = "Intergroup contact",
        "pi" = "Perceived injustice",
        "ca" = "Collective action",
        "ps" = "Policy support"
      ),
      y_name = recode_factor(
        y_var,
        "pi" = "Perceived injustice",
        "ca" = "Collective action",
        "ps" = "Policy support"
      )
    )
  

# Figure 3a ---------------------------------------------------------------

  # Prepare
  d_3a <- results %>% 
    filter(x_var == "ic") %>% 
    mutate(
      text = paste0("*I* = ", I, ", *J* = ", J, ", *N* = ", f_comma(N))
    )
  
  # Visualize
  f_3a <- ggplot(d_3a, aes(x = r_mean)) +
    geom_density(
      aes(fill = y_name),
      colour = NA,
      adjust = 2,
      n = 1e4,
      alpha = 1.0
    ) +
    annotate(
      geom = "polygon",
      x = c(0, Inf, Inf,   0),
      y = c(0,   0, Inf, Inf),
      fill = "white",
      alpha = 0.6
    ) +
    geom_richtext(
      data = d_3a %>% 
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
      data = d_3a %>% group_by(y_name) %>% summarise(text = unique(text)),
      aes(label = text),
      x = 0.21, y = 24,
      hjust = 1, vjust = 1,
      size = 10*0.8/.pt,
      label.colour = NA
    ) + 
    geom_vline(xintercept = 0, linetype = "dashed", size = 0.5) +
    scale_x_continuous(breaks = seq(-1, 1, 0.1)) +
    scale_fill_manual(
      values = c(
        "Perceived injustice" = "#648FFF", 
        "Collective action" = "#DC267F", 
        "Policy support" = "#FFB000"
      )
    ) +
    facet_grid(y_name ~ .) +
    coord_cartesian(xlim = c(-0.21, 0.21), ylim = c(0, 24), expand = FALSE) +
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
      y = "Pr"
    )
  
  
# Figure 3b ---------------------------------------------------------------

  # Prepare
  d_3b <- results %>% 
    filter(x_var == "ic") %>% 
    mutate(
      text = paste0("*I* = ", I, ", *J* = ", J, ", *N* = ", f_comma(N))
    ) %>% 
    group_by(y_name, text) %>% 
    summarise(across(c(mu, tau_jj), median))
  
  # Visualize
  f_3b <- d_3b %>% 
    crossing(r = seq(-1, 1, 0.001)) %>% 
    mutate(p = dnorm(r_to_z(r), mu, tau_jj)) %>% 
    ggplot(., aes(x = r, y = p, group = y_name)) +
    geom_area(
      aes(fill = y_name),
      colour = NA
    ) +
    geom_rect(
      data = d_3b %>% 
        group_by(y_name) %>% 
        mutate(
          r = z_to_r(mu),
          p = 0,
          .lower = qnorm(0.1, mu, tau_jj),
        ),
      aes(xmax = .lower),
      xmin = -Inf,
      ymin = 0,
      ymax = Inf,
      fill = "white",
      alpha = 0.6
    ) +
    geom_rect(
      data = d_3b %>% 
        group_by(y_name) %>% 
        mutate(
          r = z_to_r(mu),
          p = 0,
          .upper = qnorm(0.9, mu, tau_jj)
        ),
      aes(xmin = .upper),
      xmax = Inf,
      ymin = 0,
      ymax = Inf,
      fill = "white",
      alpha = 0.6
    ) +
    geom_richtext(
      data = d_3a %>% group_by(y_name) %>% summarise(r = mean(r_mean)),
      label = "80%",
      y = 0,
      vjust = 0,
      size = 10*0.8/.pt,
      colour = "white", fill = NA, label.colour = NA
    ) + 
    geom_richtext(
      data = d_3b %>% group_by(y_name) %>% summarise(text = unique(text)),
      aes(label = text),
      x = 0.42, y = 4.1,
      hjust = 1, vjust = 1,
      size = 10*0.8/.pt,
      label.colour = NA
    ) + 
    geom_vline(xintercept = 0, linetype = "dashed", size = 0.5) +
    scale_x_continuous(breaks = seq(-1, 1, 0.2)) +
    scale_fill_manual(
      values = c(
        "Perceived injustice" = "#648FFF", 
        "Collective action" = "#DC267F", 
        "Policy support" = "#FFB000"
      )
    ) +
    facet_grid(y_name ~ .) +
    coord_cartesian(xlim = c(-0.42, 0.42), ylim = c(0, 4.1), expand = FALSE) +
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
      y = "Pr"
    )


# Combine -----------------------------------------------------------------

  # Combine figures
  f_3a + f_3b + plot_layout(ncol = 1) + plot_annotation(tag_levels = "A")
  

# Export ------------------------------------------------------------------

  # Export figure (as .pdf)
  ggsave(
    "figures/figure-3.pdf",
    width = 15.14, height = 15.14/4*5, units = "cm",
    device = cairo_pdf
  )
  
  # Export figure (as .png)
  ggsave(
    "figures/figure-3.png",
    width = 15.14, height = 15.14/4*5, units = "cm",
    dpi = 600
  )
