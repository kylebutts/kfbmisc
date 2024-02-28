#' Source: https://github.com/olihawkins/pilot/
# %%
#| warning: false
library(kfbmisc)
library(tidyverse)
library(here)
library(pilot)

# %% 
df <- 
  read_csv(
    here("gallery/data/stacked-column-chart.csv"),
    show_col_types = FALSE
  ) |>
  mutate(
    year = as.character(year),
    nationality = factor(nationality, levels = c("Non-EU", "EU", "British"))
  )

# %% 
(plot <- ggplot(data = df) +
  geom_col(
    mapping = aes(
      x = year,
      y = estimate,
      fill = nationality
    ), 
    width = 0.8
  ) +
  labs(
    title = "Immigration is stable but the composition has changed",
    subtitle = "Immigration by nationality in each year ending September (000s)",
    caption = "Source: ONS, Provisional LTIM estimates",
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  scale_x_discrete() +
  scale_y_continuous(
    limits = c(0, 700),
    breaks = seq(0, 700, 100),
    expand = c(0, 0)
  ) +
  # Use scale_fill_manual and pilot_color to set category colors
  scale_fill_manual(values = c(
    "British" = pilot_color("yellow"),
    "EU" = pilot_color("navy"),
    "Non-EU" = pilot_color("blue")
  )) +
  theme_kyle(
    base_size = 14,
    grid = "h"
  ) + 
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 5, 0),
    legend.justification = c(0, 1),
    legend.location = "plot"
  )
)

# %% 
tikzsave(
  filename = here("gallery/figures/stacked-column-chart.pdf"),
  plot = plot, width = 8, height = 5,
  create_png = TRUE
)
