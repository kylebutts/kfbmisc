# %%
#| warning: false
# library(kfbmisc)
library(tidyverse)
library(here)
library(ggtext)
devtools::load_all()

reg <- lm(body_mass_g ~ bill_length_mm, palmerpenguins::penguins)

(plot <- ggplot() +
  geom_point(
    aes(x = body_mass_g, y = bill_length_mm),
    data = palmerpenguins::penguins,
    shape = 21
  ) +
  geom_smooth(
    aes(x = body_mass_g, y = bill_length_mm),
    data = palmerpenguins::penguins,
    color = kfbmisc::kyle_color("blue"),
    fill = colorspace::lighten(kfbmisc::kyle_color("blue"), 0.8),
    method = "lm",
    formula = y ~ x,
    linewidth = 1.5,
  ) +
  annotate(
    "label",
    label = sprintf(
      "Slope = $%0.2f$ $(%0.2f)$",
      coef(reg)["bill_length_mm"],
      se(reg)["bill_length_mm"]
    ),
    x = 2650,
    y = 58.5,
    hjust = 0,
    vjust = 1,
    text.color = tailwind_color("zinc-800"),
    border.color = tailwind_color("zinc-300"),
    label.padding = unit(2, "pt"),
    label.r = unit(0, "pt"),
  ) +
  labs(
    x = "Body Mass (g)",
    y = "Bill Length (mm)",
    title = "Palmer's Penguin data"
  ) +
  kfbmisc::theme_kyle(base_size = 12))

# %%
tikzsave(
  filename = here("gallery/figures/reg_line_with_label.pdf"),
  plot,
  width = 8,
  height = 5,
  create_png = TRUE
)
