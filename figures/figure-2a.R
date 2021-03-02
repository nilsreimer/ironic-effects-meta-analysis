# Notes -------------------------------------------------------------------

  #########################################################################
  # Thanks to Claus Wilke for implementing the "Interrupted Goode         #
  # homolosine" projection in R.                                          #
  #########################################################################

# Library -----------------------------------------------------------------
  
  # Load packages
  library(tidyverse); library(sf); library(rworldmap); library(numform)


# Prepare map -------------------------------------------------------------

  # Load map
  world_sf <- st_as_sf(getMap(resolution = "low"))
  
  # Prepare map projection
  crs_goode <- "+proj=igh"
  goode_outline <- list(cbind(
    longs <- c(
      rep(180, 181), 
      rep(c(80.01, 79.99), each = 91), 
      rep(c(-19.99, -20.01), each = 91), 
      rep(c(-99.99, -100.01), each = 91),
      rep(-180, 181), 
      rep(c(-40.01, -39.99), each = 91),
      180 
    ), 
    lats <- c(
      90:-90,
      -90:0, 0:-90, 
      -90:0, 0:-90, 
      -90:0, 0:-90, 
      -90:90, 
      90:0, 0:90, 
      90
    )
  )) %>%
    st_polygon() %>%
    st_sfc(
      crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    )
  goode_outline <- st_transform(goode_outline, crs = crs_goode)
  xlim <- st_bbox(goode_outline)[c("xmin", "xmax")]*1.1
  ylim <- st_bbox(goode_outline)[c("ymin", "ymax")]*1.1
  goode_encl_rect <- 
    list(
      cbind(
        c(xlim[1], xlim[2], xlim[2], xlim[1], xlim[1]), 
        c(ylim[1], ylim[1], ylim[2], ylim[2], ylim[1])
      )
    ) %>%
    st_polygon() %>%
    st_sfc(crs = crs_goode)
  goode_without <- st_difference(goode_encl_rect, goode_outline)
  
  
# Prepare data ------------------------------------------------------------

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
  
  # Join with map data 
  world_sf$N <- dm %>%
    filter(!(country %in% c("Hong Kong", "Singapore"))) %>% 
    group_by(country) %>% 
    summarize(I = n(), J = n_distinct(id), N = sum(n)) %>%
    ungroup() %>% 
    mutate(
      country = case_when(
        country == "USA" ~ "United States of America",
        country == "UK" ~ "United Kingdom",
        country == "Serbia" ~ "Republic of Serbia",
        country == "Palestine" ~ "West Bank",
        TRUE ~ country
      ),
      country = factor(country, levels(world_sf$ADMIN))
    ) %>% 
    left_join(
      tibble(country = world_sf$ADMIN),
      .,
      by = "country"
    ) %>% 
    pull(N)
    
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
    st_as_sf(coords = c("long", "lat"), crs = st_crs(world_sf))
  

# Visualize ---------------------------------------------------------------

  # Figure 2a
  f_2a <- ggplot(world_sf) + 
    geom_sf(
      aes(fill = N), 
      color = "black", 
      size = 0.5/.pt, 
      alpha = 1
    ) +
    geom_sf(
      data = cities,
      colour = "black",
      size = 1.5
    ) +
    geom_sf(
      data = cities,
      aes(colour = N),
      size = 1
    ) +
    geom_sf(
      data = goode_without,
      color = NA,
      fill = "white"
    ) +
    geom_sf(
      data = goode_outline,
      color = "black",
      fill = NA,
      size = 0.5/.pt
    ) +
    scale_colour_viridis_c(
      option = "B",
      limits = c(min(world_sf$N, na.rm = TRUE), max(world_sf$N, na.rm = TRUE)),
      direction = -1,
      breaks = c(10, 100, 1000, 10000, 100000),
      labels = f_comma(c(10, 100, 1000, 10000, 100000)),
      trans = "log",
      na.value = "grey92",
      guide = NULL
    ) +
    scale_fill_viridis_c(
      option = "B",
      limits = c(min(world_sf$N, na.rm = TRUE), max(world_sf$N, na.rm = TRUE)),
      direction = -1,
      breaks = c(10, 100, 1000, 10000, 100000),
      labels = f_comma(c(10, 100, 1000, 10000, 100000)),
      trans = "log",
      na.value = "grey92"
    ) +
    guides(
      fill = guide_colourbar(
        title = "Number of participants",
        title.position = "top",
        label.position = "bottom",
        label.hjust = 0,
        ticks.linewidth = 1,
        draw.ulim = TRUE,
        barheight = unit(10, "pt"),
        barwidth = unit(2, "in")
      )
    ) +
    coord_sf(
      crs = crs_goode,
      xlim = 0.95 * xlim,
      ylim = 0.95 * ylim,
      expand = FALSE
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "bottom",
      legend.justification = c(0, 0),
      panel.background = element_rect(fill = "#56B4E950", color = "white", size = 1),
      panel.grid = element_blank()
    )


# Remove ------------------------------------------------------------------
  
  # Remove objects
  rm(list = setdiff(ls(), "f_2a"))
  
  # Remove packages
  detach(package:sf); detach(package:rworldmap)
