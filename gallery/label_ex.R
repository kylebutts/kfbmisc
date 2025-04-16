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
    color = kfbmisc::kyle_color("blue"),
    fill = colorspace::lighten(kfbmisc::kyle_color("blue"), 0.8),
    data = palmerpenguins::penguins,
    method = "lm",
    formula = y ~ x,
    linewidth = 1.5,
  ) +
  # geom_textbox(
  #   label = sprintf(
  #     "Slope = $%0.2f$ $(%0.2f)$",
  #     coef(reg)["bill_length_mm"],
  #     se(reg)["bill_length_mm"]
  #   ),
  #   x = 2650,
  #   y = 58.5,
  #   color = "black",
  #   max.width = unit(0.4, "npc")
  # ) +
  kfbmisc::kyle_textbox(
    label = sprintf(
      "Slope = $%0.2f$ $(%0.2f)$",
      coef(reg)["bill_length_mm"],
      se(reg)["bill_length_mm"]
    ),
    x = 2650,
    y = 58.5,
    box.size = unit(0.5, "pt"),
    width = unit(0.19, "npc"),
    fill = "white"
  ) +
  # kfbmisc::kyle_label(
  #   label = sprintf(
  #     "Slope = $%0.2f$ $(%0.2f)$",
  #     coef(reg)["bill_length_mm"],
  #     se(reg)["bill_length_mm"]
  #   ),
  #   x = 2650,
  #   y = 52.5,
  # ) +
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
