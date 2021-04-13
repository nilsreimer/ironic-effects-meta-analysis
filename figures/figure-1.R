rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------

  # Load packages
  library(tidyverse); library(glue); library(ggtext); library(numform)


# Prepare -----------------------------------------------------------------

  # Import data
  dl <- read_rds("data/dl.rds")
  
  # Compile records
  records <- bind_rows(
    read_csv("records/records-cleaned.csv") %>% 
      distinct(id) %>% 
      mutate(stage = 1L), 
    read_csv("records/records-screened.csv") %>% 
      distinct(id) %>% 
      mutate(stage = 2L),
    read_csv("records/records-coded.csv") %>% 
      distinct(id) %>% 
      mutate(stage = 3L)
  ) %>% 
    anti_join(
      bind_rows(
        read_csv("records/results/unpublished-studies.csv") %>% distinct(id),
        read_csv("records/results/citing-studies.csv") %>% distinct(id)
      ),
      by = "id"
    ) %>% 
    bind_rows(
      dl %>% 
        distinct(id, publication_status) %>% 
        mutate(stage = 4L),
      dl %>% 
        distinct(id, publication_status) %>% 
        mutate(stage = 5L)
    ) %>% 
    mutate(
      source = case_when(
        is.na(publication_status) ~ "database",
        id %in% unique(read_csv("records/results/citing-studies.csv")$id) ~ "citing",
        publication_status == "unpublished" ~ "unpublished",
        TRUE ~ "database"
      )
    ) %>% 
    bind_rows(
      read_csv("records/results/unpublished-studies.csv") %>% 
        distinct(id) %>% 
        anti_join(
          dl %>% filter(publication_status == "published") %>% distinct(id),
          by = "id"
        ) %>% 
        mutate(stage = 3L, source = "unpublished"),
      read_csv("records/results/citing-studies.csv") %>% 
        distinct(id) %>% 
        mutate(stage = 3L, source = "citing")
    ) %>% 
    count(stage, source) %>% 
    pivot_wider(
      names_from = source,
      values_from = n
    )

  # Summarize records
  counts <- records %>% 
    transmute(
      stage,
      n_included = case_when(
        stage <= 3 ~ database,
        stage >= 3 ~ database + citing + unpublished
      ),
      p_included = case_when(
        stage == 1L ~ 1.0,
        stage <= 3L ~ n_included/lag(n_included),
        stage == 4L ~ (n_included)/lag(database + citing + unpublished),
        stage == 5L ~ n_included/lag(n_included)
      ) %>% f_prop2percent(., digits = 0),
      n_excluded = case_when(
        stage == 1L ~ 0L,
        stage <= 3L ~ lag(n_included) - n_included,
        stage == 4L ~ lag(database + citing + unpublished) - (n_included),
        stage == 5L ~ lag(n_included) - (n_included),
      ),
      p_excluded = case_when(
        stage == 1L ~ 1.0,
        stage == 4L ~ n_excluded / lag(database + citing + unpublished),
        TRUE ~ n_excluded / lag(n_included)
      ) %>% f_prop2percent(., digits = 0)
    )
  

# Visualize ---------------------------------------------------------------

  # Create boxes
  boxes <- bind_rows(
    tibble(
      xmin = rep(27, 6),
      xmax = xmin + 18,
      ymin = seq(46, 6, -8),
      ymax = ymin - 5
    ),
    tibble(
      xmin = c(1, 14),
      xmax = xmin + 11,
      ymin = 46,
      ymax = 46 - 5
    )
  )
  
  # Create arrows
  arrows <- bind_rows(
    tibble(
      x    = 27 + 9,
      y    = seq(41, 6, -8),
      xend = x,
      yend = y - 3
    ),
    tibble(
      x    = 27 + 18,
      y    = seq(38 - 2.5, 22 - 2.5, -8),
      xend = x + 3,
      yend = y
    )
  )
  
  # Create labels
  labels <- bind_rows(
    tibble(
      x    = rep(27 + 9, 6),
      y    = seq(46 - 2.5, 6 - 2.5, -8),
      text = c(
        "Retrieve records from<br>electronic databases",
        "Screen records<br>(title, abstract, keywords)",
        "Code eligibility criteria<br>(full-text manuscripts)",
        "Extract effect sizes",
        "Code moderators",
        "Analyze data"
      ),
      hjust = 0.5
    ),
    tibble(
      x    = 27 + 9,
      y    = seq(46 - 6.5, 14 - 6.5, -8),
      text = glue("{counts$n_included} ({counts$p_included})"),
      hjust = 0
    ),
    tibble(
      x    = 27 + 21,
      y    = seq(38 - 2.5, 22 - 2.5, -8),
      text = glue("{counts$n_excluded[2:4]} ({counts$p_excluded[2:4]})"),
      hjust = 0
    ),
    tibble(
      x = c(1, 14) + 5.5,
      y = 46 - 2.5,
      text = c(
        "Find unpublished<br>studies",
        "Find citing<br>studies"
      ),
      hjust = 0.5
    ),
    tibble(
      x    = c(1, 14) + 5.5,
      y    = 46 - 6.5,
      text = c(
        as.character(records$unpublished[3]),
        as.character(records$citing[3])
      ),
      hjust = 0
    )
  )

  # Create flow chart
  ggplot(NULL) + 
  geom_path(
    data = tibble(
      x = c(6.5, 6.5, 27, 19.5, 19.5, 27),
      y = c(41, 19.5, 19.5, 41, 19.5, 19.5),
      group = rep(1:2, each = 3)
    ),
    aes(x = x, y = y, group = group),
    size = 1/.pt,
    linejoin = "mitre",
    arrow = arrow(
      length = unit(4, "pt"),
      type = "closed"
    )
  ) +
  geom_segment(
    data = arrows,
    aes(x = x, y = y, xend = xend, yend = yend),
    size = 1/.pt,
    linejoin = "mitre",
    arrow = arrow(
      length = unit(4, "pt"),
      type = "closed"
    )
  ) +
  geom_rect(
    data = boxes,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    colour = "black", fill = "white", size = 1/.pt
  ) +
  geom_richtext(
    data = labels,
    aes(x = x, y = y, label = text, hjust = hjust),
    colour = "black", fill = NA, label.colour = NA,
    size = 10/.pt
  ) +
  scale_x_continuous(
    breaks = seq(0, 56, 4),
    minor_breaks = seq(0, 56, 1)
  ) +
  scale_y_continuous(
    breaks = seq(47, 0, -4),
    minor_breaks = seq(47, 0, -1)
  ) +
  coord_equal(xlim = c(0, 56), ylim = c(0, 47), expand = FALSE) +
  theme_void(base_size = 10)


# Export ------------------------------------------------------------------

  # Export figure (as .pdf)
  ggsave(
    "figures/figure-1.pdf",
    width = 6.5, height = 4.7, units = "in",
    device = cairo_pdf
  )
  
  # Export figure (as .png)
  ggsave(
    "figures/figure-1.png",
    width = 6.5, height = 4.7, units = "in",
    dpi = 600
  )      
