#' Source: https://github.com/olihawkins/pilot/
# %%
#| warning: false
library(kfbmisc)
library(tidyverse)
library(here)

df <-
  read_csv(
    here("gallery/data/stacked_column_chart.csv"),
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
  scale_fill_manual(
    values = c(
      "British" = kyle_color("rose"),
      "EU" = kyle_color("navy"),
      "Non-EU" = kyle_color("blue")
    ),
    guide = guide_legend(
      keywidth = unit(8, "pt"),
      keyheight = unit(2, "pt")
    )
  ) +
  theme_kyle(
    base_size = 12,
    grid = "hv",
    legend = "top"
  ) +
  theme(
    legend.key.spacing.x = unit(12, "pt"),
  ))

# %%
tikzsave(
  filename = here("gallery/figures/stacked_column_chart.pdf"),
  plot = plot,
  width = 8,
  height = 5,
  create_png = TRUE
)
