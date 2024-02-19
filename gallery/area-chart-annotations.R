#' Source: https://github.com/olihawkins/pilot/
# %%
#| warning: false
library(kfbmisc)
library(tidyverse)
library(here)
library(pilot)

# %% 
df <- read_csv(
    here("gallery/data/area-chart-annotations.csv"),
    show_col_types = FALSE
  ) |>
  pivot_longer(
    cols = -date,
    names_to = "energy_source",
    values_to = "gwh"
  ) |> 
  mutate(
    factor(energy_source, levels = c("other", "renewables"))
  )

# %% 
(plot <- 
  ggplot(
    data = df,
    mapping = aes(x = date, y = gwh, fill = energy_source)
  ) +
  geom_area() +
  labs(
    title = "Renewables are growing as a share of electricity generation",
    subtitle = "Electricity generation by fuel type in the United Kingdom from 1996 to 2020, GWh",
    caption = "Source: BEIS, Digest of UK Energy Statistics, Table 5.3",
    x = NULL, y = NULL
  ) +
  scale_y_continuous(
    label = scales::label_comma(),
    limits = c(0, 402000),
    breaks = seq(0, 400000, 100000),
    expand = c(0, 0.05)
  ) +
  scale_x_date(
    expand = c(0, 0)
  ) +
  annotate(
    geom = "text",
    x = as.Date("2015-01-01"),
    y = 200000,
    label = "Non-renewable",
    color = "#ffffff",
    hjust = 0,
    size = 14 / .pt
  ) +
  annotate(
    geom = "text",
    x = as.Date("2016-04-01"),
    y = 40000,
    label = "Renewable",
    color = "#202020",
    hjust = 0,
    size = 14 / .pt
  ) +
  scale_fill_manual(values = c(
    "renewables" = pilot_color("green"),
    "other" = pilot_color("navy")
  )) + 
  theme_kyle(
    base_size = 14, grid = "h"
  ) + 
  theme(
    plot.title.position = "plot",
    legend.position = "none"
  )
)

# %% 
tikzsave(
  filename = here("gallery/figures/area-chart-annotations.pdf"),
  plot = plot, width = 8, height = 6,
  create_png = TRUE,
)
