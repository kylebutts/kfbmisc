#' Source: https://github.com/olihawkins/pilot/
# %%
#| warning: false
library(kfbmisc)
library(tidyverse)
library(scales)
library(here)

df <- read_csv(
  here("gallery/data/scatter_chart_facets.csv"),
  show_col_types = FALSE
) |>
  filter(!is.na(classification))

settlement_classes <- c(
  "London",
  "Other city",
  "Large town",
  "Medium town",
  "Small town",
  "Village"
)

df$classification <- factor(df$classification, levels = settlement_classes)

# %%
(plot <-
  ggplot(
    data = df,
    mapping = aes(
      x = median_age,
      y = turnout,
      color = classification
    )
  ) +
  geom_point(
    shape = 16,
    size = 2,
    alpha = 0.6
  ) +
  facet_wrap(~classification) +
  labs(
    title = "Turnout was higher in older, less urban constituencies",
    subtitle = "Constituencies by age, turnout and settlement class, 2017",
    x = "Median age",
    y = "Turnout",
    color = "Settlement class"
  ) +
  scale_x_continuous(
    limits = c(25, 55),
    breaks = seq(25, 55, 10)
  ) +
  scale_y_continuous(
    limits = c(0.5, 0.8),
    label = scales::percent_format(accuracy = 1, suffix = "\\%")
  ) +
  scale_color_manual(
    values = c(
      "London" = kyle_color("navy"),
      "Other city" = kyle_color("magenta"),
      "Large town" = kyle_color("purple"),
      "Medium town" = kyle_color("blue"),
      "Small town" = kyle_color("green"),
      "Village" = kyle_color("yellow")
    )
  ) +
  theme_kyle(
    base_size = 12,
    axes = "",
    grid = "hv",
    grid_minor = ""
  ) +
  theme(
    legend.position = "none"
  ))

# %%
tikzsave(
  filename = here("gallery/figures/scatter_chart_facets.pdf"),
  plot,
  width = 8,
  height = 6,
  create_png = TRUE
)
