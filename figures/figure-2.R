rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------

  # Load packages
  library(tidyverse); library(numform)


# Prepare -----------------------------------------------------------------

  # Import data
  dl <- read_rds("data/dl.rds")
  
  # Select variables
  dm <- dl %>% distinct(id, sample, n, country)
  
  # Separate samples with multiple countries (if possible)
  dm <- dm %>% 
    filter(
      !(id == 829L & sample == 1L),
      !(id == 956L & sample == 2L),
      !(id == 3054L & sample == 1L)
    ) %>% 
    bind_rows(
      tribble(
          ~id, ~sample,   ~n, ~country,
         829L,      1L,  55L, "Finland",
         829L,      1L,  61L, "France",
         829L,      1L,  89L, "Germany",
         829L,      1L, 100L, "Norway",
         829L,      1L, 273L, "Sweden",
         829L,      1L, 158L, "Netherlands",
         956L,      2L, 310L, "Germany",
         956L,      2L, 123L, "UK",
        3054L,      1L, 118L, "Chile",
        3054L,      1L, 127L, "Chile",
        3054L,      1L, 110L, "Germany",
        3054L,      1L, 112L, "Kosovo",
        3054L,      1L,  89L, "Poland",
        3054L,      1L,  95L, "Serbia",
        3054L,      1L,  27L, "Spain",
        3054L,      1L,  89L, "Switzerland",
        3054L,      1L, 127L, "UK",
        3054L,      1L, 106L, "USA"
      )
    ) %>% 
    arrange(id, sample)
  
  # Remove samples with multiple countries (if not)
  dm <- dm %>% filter(
    !(id == 3054L & sample == 11L), 
    !(id == 303L & sample == 1L)
  )
  
  # Prepare map data 
  areas <- dm %>%
    filter(!(country %in% c("Hong Kong", "Singapore"))) %>% 
    group_by(country) %>% 
    summarize(I = n(), J = n_distinct(id), N = sum(n)) %>%
    left_join(
      map_data("world"),
      .,
      by = c("region" = "country")
    )
    
  # Prepare map data
  cities <- dm %>%
    filter(country %in% c("Hong Kong", "Singapore")) %>% 
    group_by(country) %>% 
    summarize(I = n(), J = n_distinct(id), N = sum(n)) %>% 
    left_join(
      tribble(
           ~country,     ~lat,      ~long,
        "Hong Kong", 22.30271, 114.177216,
        "Singapore", 1.283333, 103.833333
      )
    ) %>% 
    rename(region = country)


# Visluaize ---------------------------------------------------------------

  # Figure 2
  ggplot(areas)  +
    geom_polygon(
      aes(x = long, y = lat, group = group, fill = N),
      colour = "grey20",
      size = 0.1
    ) +
    geom_point(
      data = cities,
      aes(x = long, y = lat, fill = N),
      colour = "grey20",
      shape = "circle filled",
      size = 3
    ) +
    scale_fill_viridis_c(
      option = "A",
      direction = -1,
      breaks = c(100, 1000, 10000, 100000),
      labels = f_comma(c(100, 1000, 10000, 100000)),
      trans = "log",
      na.value = "white"
    ) +
    guides(
      fill = guide_colourbar(
        title = "Number of Participants",
        title.position = "top",
        label.position = "bottom",
        label.hjust = 0,
        ticks.linewidth = 1,
        draw.ulim = TRUE,
        barwidth = unit(8, "cm")
      )
    ) +
    coord_equal(
      xlim = c(-125, 180),
      ylim = c(-60, 90),
      expand = FALSE
    ) +
    theme_void(base_size = 16) +
    theme(
      legend.position = "bottom",
      legend.justification = "left",
    )


# Export ------------------------------------------------------------------

  # Export figure (as .png)
  ggsave(
    "presentation/map-ppt.png",
    width = 21.91, height = 16.14, units = "cm", dpi = 600,
    type = "cairo-png"
  )
    