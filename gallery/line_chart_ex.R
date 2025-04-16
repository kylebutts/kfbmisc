#' Source: https://github.com/olihawkins/pilot/
# %%
#| warning: false
library(kfbmisc)
library(tidyverse)
library(here)

df <- read_csv(
  here("gallery/data/line_chart.csv"),
  show_col_types = FALSE
)

# %%
(plot <-
  ggplot(
    data = df
  ) +
  # Add a line geometry to draw lines
  geom_line(
    mapping = aes(
      x = quarter,
      y = estimate,
      color = flow
    ),
    linewidth = 1.6
  ) +
  # Set labels for the axes, legend, and caption, but don't set titles here
  labs(
    title = "Net migration fell after the EU referendum",
    subtitle = "International migration in the year ending each quarter",
    caption = "Source: ONS, Provisional LTIM estimates",
    y = "Thousands of people",
    x = NULL,
    color = NULL
  ) +
  scale_x_date(
    breaks = lubridate::as_date(
      paste0(seq(2010, 2020, 2), "-01-01")
    ),
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    breaks = seq(0, 800, 200),
    limits = c(0, 800),
    expand = c(0, 0)
  ) +
  scale_color_manual(
    values = c(
      "Immigration" = kyle_color("navy"),
      "Net migration" = kyle_color("blue")
    )
  ) +
  theme_kyle(
    base_size = 12,
    axes = "b",
    grid = "h",
  ) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(1, 0.99),
    legend.justification = c(1, 1),
    legend.direction = "horizontal"
  ))

# %%
tikzsave(
  filename = here("gallery/figures/line_chart.pdf"),
  plot = plot,
  width = 8,
  height = 5.5,
  create_png = TRUE
)
