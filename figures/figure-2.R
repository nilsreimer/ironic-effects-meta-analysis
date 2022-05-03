rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------

  # Load packages
  library(tidyverse); library(numform); library(patchwork)

# Visualize ---------------------------------------------------------------

  # Figure 2a
  source("figures/figure-2a.R")


# Prepare -----------------------------------------------------------------

  # Import data
  dl <- read_rds("data/dl.rds")
  
  # Select variables
  dl <- dl %>% 
    select(
      id,
      sample,
      n,
      country:publication_status 
    ) %>% 
    distinct()

# Visualize ---------------------------------------------------------------

  # Figure 2b
  f_2b <- dl %>% 
    mutate(
      continent = case_when(
        str_detect(continent, ",") ~ "Mixed",
        TRUE ~ continent
      )
    ) %>% 
    group_by(continent) %>% 
    summarize(
      N = sum(n),
      I = n(),
      J = n_distinct(id)
    ) %>% 
    mutate(
      P = I/sum(I)
    ) %>% 
    arrange(P) %>% 
    mutate(
      continent = factor(continent, levels = c("Mixed", continent[continent != "Mixed"]))
    ) %>% 
    ggplot(., aes(x = P, y = continent)) +
    geom_col(
      aes(fill = if_else(continent == "Mixed", "grey82", "black")),
      width = 0.8
    ) +
    geom_text(
      aes(
        label = I,
        colour = if_else(P < 0.10, "black", "white"),
        hjust = if_else(P < 0.10, -0.25, 1.25)
      ),
      size = 9/.pt
    ) +
    scale_x_continuous(
      labels = scales::percent_format(accuracy = 10),
      expand = c(0, 0)
    ) +
    scale_colour_identity() + 
    scale_fill_identity() +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "none",
      plot.title = element_text(colour = "black", face = "bold"),
      axis.text = element_text(colour = "black"),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      panel.grid.major.x = element_line(colour = "grey92")
    ) +
    labs(
      tag = "B",
      title = "Continent"
    )

  # Figure 2c
  f_2c <- dl %>%
    mutate(
      study_setting = case_when(
        str_detect(study_setting, ",") ~ "Other",
        study_setting == "Slavery" ~ "Post-Slavery",
        study_setting == "Colonization" ~ "(Post-)Colonial",
        TRUE ~ study_setting
      )
    ) %>% 
    group_by(study_setting) %>% 
    summarize(
      N = sum(n),
      I = n(),
      J = n_distinct(id)
    ) %>% 
    mutate(
      P = I/sum(I)
    ) %>% 
    arrange(P) %>% 
    filter(!is.na(study_setting)) %>% 
    mutate(
      study_setting = factor(study_setting, levels = c("Other", study_setting[study_setting != "Other"]))
    ) %>% 
    ggplot(., aes(x = P, y = study_setting)) +
    geom_col(
      aes(fill = if_else(study_setting == "Other", "grey82", "black")),
      width = 0.8
    ) +
    geom_text(
      aes(
        label = I,
        colour = if_else(P < 0.10, "black", "white"),
        hjust = if_else(P < 0.10, -0.25, 1.25)
      ),
      size = 9/.pt
    ) +
    scale_x_continuous(
      labels = scales::percent_format(accuracy = 10),
      expand = c(0, 0)
    ) +
    scale_colour_identity() + 
    scale_fill_identity() +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "none",
      plot.title = element_text(colour = "black", face = "bold"),
      axis.text = element_text(colour = "black"),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      panel.grid.major.x = element_line(colour = "grey92")
    ) +
    labs(
      title = "Study Setting"
    )
  
  # Figure 2d
  f_2d <- dl %>%
    mutate(
      study_design = recode(
        study_design,
        "experimental (random assignment)" = "experimental",
        "quasi-experimental (no random assignment)" = "quasi-experimental",
        "observational, longitudinal" = "correlational,\nlongitudinal",
        "observational, cross-sectional" = "correlational,\ncross-sectional"
      )
    ) %>%
    group_by(study_design) %>% 
    summarize(
      N = sum(n),
      I = n(),
      J = n_distinct(id)
    ) %>% 
    mutate(
      P = I/sum(I)
    ) %>% 
    arrange(P) %>% 
    mutate(
      study_design = factor(study_design, levels = study_design)
    ) %>% 
    ggplot(., aes(x = P, y = study_design)) +
    geom_col(
      fill = "black",
      width = 0.8
    ) +
    geom_text(
      aes(
        label = I,
        colour = if_else(P < 0.10, "black", "white"),
        hjust = if_else(P < 0.10, -0.25, 1.25)
      ),
      size = 9/.pt
    ) +
    scale_x_continuous(
      labels = scales::percent_format(accuracy = 10),
      expand = c(0, 0)
    ) +
    scale_colour_identity() + 
    scale_fill_identity() +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "none",
      plot.title = element_text(colour = "black", face = "bold"),
      axis.text = element_text(colour = "black"),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      panel.grid.major.x = element_line(colour = "grey92")
    ) +
    labs(
      title = "Study Design"
    )
  
  # Figure 2d
  f_2e <- dl %>%
    mutate(
      study_sample = recode(
        study_sample,
        "convenience sample, non-students (no random sampling)" = "convenience sample,\nnot students",
        "convenience sample, students (no random sampling)" = "convenience sample,\nstudents",
        "probability or representative sample" = "probability/representative\nsample "
      )
    ) %>%
    group_by(study_sample) %>% 
    summarize(
      N = sum(n),
      I = n(),
      J = n_distinct(id)
    ) %>% 
    mutate(
      P = I/sum(I)
    ) %>% 
    arrange(P) %>% 
    mutate(
      study_sample = factor(study_sample, levels = study_sample)
    ) %>% 
    ggplot(., aes(x = P, y = study_sample)) +
    geom_col(
      fill = "black",
      width = 0.8
    ) +
    geom_text(
      aes(
        label = I,
        colour = if_else(P < 0.10, "black", "white"),
        hjust = if_else(P < 0.10, -0.25, 1.25)
      ),
      size = 9/.pt
    ) +
    scale_x_continuous(
      labels = scales::percent_format(accuracy = 10),
      expand = c(0, 0)
    ) +
    scale_colour_identity() + 
    scale_fill_identity() +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "none",
      plot.title = element_text(colour = "black", face = "bold"),
      axis.text = element_text(colour = "black"),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      panel.grid.major.x = element_line(colour = "grey92")
    ) +
    labs(
      title = "Study Sample"
    )

# Combine -----------------------------------------------------------------
  
  # Combine figures
  f_2a + f_2b + f_2c + f_2d + f_2e + plot_layout(
    heights = c(2, 1, 1),
    widths = c(0, 1, 1),
    design = "
    AAA
    #BC
    #DE
    "
  ) + plot_annotation(
    theme = theme(plot.tag = element_text(face = "bold"))
  )

# Export ------------------------------------------------------------------
  
  # Export figure (as .pdf)
  ggsave(
    "figures/figure-2.pdf",
    width = 6.5, height = 7.5, units = "in",
    device = cairo_pdf
  )
  
  # Export figure (as .png)
  ggsave(
    "figures/figure-2.png",
    width = 6.5, height = 7.5, units = "in",
    dpi = 600
  )    
    