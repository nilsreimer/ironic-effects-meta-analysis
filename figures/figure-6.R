rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------

  # Load packages
  library(tidyverse); library(tidybayes); library(patchwork)
  library(ggtext); library(numform)


# Prepare -----------------------------------------------------------------

  # Import results from meta-regression trees 
  results <- read_rds("results/results_exploratory_moderator_analyses.rds")
  
  
# Figure 6a ---------------------------------------------------------------

  # Prepare data for subfigure
  d_6a <- results %>% distinct(kk, ic_quality, study_setting, age, n)
  
  # Create boxes
  boxes <- tibble(
    xmin = c(0.0, 1.0, 1.0, 2.0, 2.0, 2.0, 2.0),
    xmax = c(0.8, 1.8, 1.8, 2.8, 2.8, 2.8, 2.8),
    ymin = c(1.5, 0.5, 2.5, 0.0, 1.0, 2.0, 3.0),
    ymax = c(2.3, 1.3, 3.3, 0.8, 1.8, 2.8, 3.8)
  )
  
  # Create arrows
  arrows <- tibble(
    x    = c(0.8, 0.8, 1.8, 1.8, 1.8, 1.8),
    xend = c(1.0, 1.0, 2.0, 2.0, 2.0, 2.0),
    y    = c(1.9, 1.9, 0.9, 0.9, 2.9, 2.9),
    yend = c(0.9, 2.9, 0.4, 1.4, 2.4, 3.4)
  )
  
  # Create labels
  labels <- tibble(
    x = c(0.4, 1.4, 1.4, 2.4, 2.4, 2.4, 2.4),
    y = c(1.9, 0.9, 2.9, 0.4, 1.4, 2.4, 3.4),
    text = c(
      glue("*I* = {sum(d_6a$n)}"),
      glue("Predictor:<br>Direct & Quality<br>*I* = {d_6a$n[3] + d_6a$n[4]}"),
      glue("Predictor:<br>Indirect | Quantity<br>*I* = {d_6a$n[1] + d_6a$n[2]}"),
      glue("Adolescents/Children<br>*I* = {d_6a$n[4]}"),
      glue("Adults<br>*I* = {d_6a$n[3]}"),
      glue("Other Settings<br>*I* = {d_6a$n[2]}"),
      glue("Short-Term Migration<br>*I* = {d_6a$n[1]}")
    )
  )
  
  # Create flow chart
  f_6a <- ggplot(NULL) + 
    geom_segment(
      data = arrows,
      aes(x = x, y = y, xend = xend, yend = yend),
      size = 1/.pt,
      linejoin = "mitre"
    ) +
    geom_rect(
      data = boxes,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      colour = "black", fill = "white", size = 1/.pt
    ) +
    geom_richtext(
      data = labels,
      aes(x = x, y = y, label = text),
      colour = "black", fill = NA, label.colour = NA,
      size = 10*0.8/.pt
    ) +
    coord_cartesian(xlim = c(0, 2.8), ylim = c(0, 4), expand = FALSE) +
    theme_classic(base_size = 10) +
    theme(
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.background = element_rect(colour = NA, fill = NA)
    )


# Figure 6b ---------------------------------------------------------------

  # Prepare data for subfigure
  d_6b <- results %>% select(.chain:.draw, kk, r)
  
  # Visualize
  f_6b <- ggplot(d_6b, aes(x = r, y = kk)) +
    geom_hline(
      yintercept = 0:4,
      colour = "grey92",
      size = 0.455
    ) +
    stat_halfeye(
      aes(fill = after_stat(x < 0), group = kk),
      point_interval = NULL,
      adjust = 2,
      n = 1e4
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", size = 0.455) +
    geom_text(
      data = d_6b %>% 
        group_by(kk) %>% 
        summarise(p = mean(r < 0), r = mean(r)) %>% 
        mutate(
          p = case_when(
            # p > 0.99 ~ ">99%",
            p > 0.99 ~ NA_character_,
            # p < 0.50 ~ paste0(round(1-p, 2)*100, "%"),
            # p > 0.50 ~ paste0(round(p, 2)*100, "%")
            TRUE ~ paste0(round(p, 2)*100, "%")
          )
        ),
      aes(label = p),
      vjust = 0,
      nudge_y = 0.05,
      size = 10*0.8/.pt,
      colour = "white" #, fill = NA, label.colour = NA
    ) +
    scale_y_reverse(breaks = 0:4) +
    scale_fill_manual(values = c("#c1d2ff", "#648fff")) +
    coord_cartesian(xlim = c(-0.375, 0.175), expand = FALSE) +
    facet_grid(. ~ "Perceived Injustice") +
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
  

# Combine -----------------------------------------------------------------

  # Combine subfigures
  f_6a + f_6b + plot_layout(widths = c(2, 1), nrow = 1)

# Export ------------------------------------------------------------------

  # Export figure (as .pdf)
  ggsave(
    "figures/figure-6.pdf",
    width = 6.5, height = 6.5/2, units = "in",
    device = cairo_pdf
  )
  
  # Export figure (as .png)
  ggsave(
    "figures/figure-6.png",
    width = 6.5, height = 6.5/2, units = "in",
    dpi = 600
  )
