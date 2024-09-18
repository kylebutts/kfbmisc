#' Source: https://github.com/olihawkins/pilot/
# %%
#| warning: false
library(kfbmisc)
library(ggplot2)
library(here)

data(mpg, package = "ggplot2")
mpg$class <- mpg$class |>
  stringr::str_to_title() |>
  stringr::str_replace("2seater", "2 Seater") |>
  stringr::str_replace("Suv", "SUV")

# %%
(plot <- ggplot(data = mpg) +
  geom_point(
    mapping = aes(x = displ, y = hwy, color = class),
    shape = 19
  ) +
  labs(
    caption = "Reproduced from Chapter 3 of R for Data Science",
    x = "Engine size in litres",
    y = "Miles per gallon",
    color = NULL
  ) +
  scale_color_kyle() +
  guides(
    color = guide_legend(nrow = 2)
  ) +
  theme_kyle(base_size = 14, axes = "") +
  theme(
    legend.position = "bottom",
    legend.justification = c(0.5, 0),
    legend.location = "plot"
  )
)

# %%
tikzsave(
  filename = here("gallery/figures/scatter-chart.pdf"),
  plot = plot, width = 8, height = 5,
  create_png = TRUE
)
