rm(list = ls())

# Load packages -----------------------------------------------------------
  library(tidyverse)


# Choose ------------------------------------------------------------------
  
  # Prior for mu (chosen for 50% to fall between -0.21 <= r <= 0.21)
  f <- function(sigma) abs(tanh(qnorm(p = 0.75, mean = 0, sd = sigma)) - 0.21) 
  sigma <- optimise(f, lower = 0, upper = 1, tol = 1e-10)$minimum
  mu_prior <- function(x, sd = sigma) dnorm(x = atanh(x), mean = 0, sd = sd)
  curve(mu_prior, from = -1, to = 1)
  
  # Prior for tau (chosen after Williams, Rast, & Bürkner, 2018)
  tau_prior <- Vectorize(function(x)  {
    if_else(x >= 0, dcauchy(x, location = 0.00, scale = 0.30)/0.5, 0)
  })
  curve(tau_prior, from = 0, to = 2)
  integrate(tau_prior, 0, 0.15)
  integrate(tau_prior, 0, 0.30)
  

# Visualise ---------------------------------------------------------------
  
  # Prior for mu
  mu_plot <- ggplot(NULL, aes(x = -1:1)) +
    geom_vline(xintercept = c(-0.21, 0.21), linetype = "dashed", colour = "grey20", size = 0.364) +
    stat_function(
      fun = mu_prior,
      geom = "area",
      n = 1e4, 
      fill = "grey20",
      alpha = 0.2,
      xlim = c(-0.21, 0.21)
    ) +
    stat_function(
      fun = mu_prior, 
      n = 1e4, 
      aes(colour = "N(0, 0.31605)"),
      size = 0.5
    ) +
    scale_x_continuous(breaks = c(seq(-1, 1, 0.5), -0.21, 0.21)) +
    scale_colour_manual(values = c("#D81B60")) +
    coord_fixed(4/3, ylim = c(0, 1.5), expand = FALSE) +
    theme_bw(base_size = 8) +
    theme(
      legend.position = c(0.95, 0.05),
      legend.justification = c(1, 0),
      legend.background = element_rect(colour = "grey20"),
      panel.grid.minor = element_blank()
    ) +
    labs(
      tag = "A",
      x = expression(tanh(mu)),
      y = expression(italic(p)),
      colour = "Prior distribution"
    )
  
  # Prior for tau
  tau_plot <- ggplot(NULL, aes(x = 0:2)) +
    geom_vline(xintercept = 0.15, linetype = "dashed", colour = "grey20", size = 0.364) +
    stat_function(
      fun = tau_prior, 
      n = 1e4, 
      aes(colour = "HC(0, 0.3)"), 
      size = 0.5
    ) +
    scale_x_continuous(breaks = c(seq(0, 2, 0.5), 0.15)) +
    scale_colour_manual(values = c("#1E88E5")) +
    coord_fixed(3/4, ylim = c(0, 3), expand = FALSE) +
    theme_bw(base_size = 8) +
    theme(
      legend.position = c(0.95, 0.95),
      legend.justification = c(1, 1),
      legend.background = element_rect(colour = "grey20"),
      panel.grid.minor = element_blank()
    ) +
    labs(
      tag = "B",
      x = expression(tau),
      y = expression(italic(p)),
      colour = "Prior distribution"
    )
  
  # Export
  cowplot::plot_grid(mu_plot, tau_plot, align = "hv")
  ggsave("protocol/prior-choice.pdf", width = 390/.pt, height = 210/.pt, units = "mm")
  