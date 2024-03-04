library(ggplot2)
library(palmerpenguins)
library(kfbmisc)
data(penguins)

library(grid)
g = grobTree(
  textGrob(
    "Flipper Length (mm)", 
    hjust = "left",
    gp = gpar(fontsize = 15, col = "black", fontface = "italic")
  )
)

ggplot(
  penguins, aes(x = body_mass_g, y = flipper_length_mm, color = species)
) +
  geom_point(size = 2) +
  labs(
    title = "Palmers Penguins",
    x = "Body mass (g)",
    y = NULL, color = NULL,
  ) +
  guides(
    custom = guide_custom(
      grob = g
    )
  ) +
  pilot::scale_color_pilot() +
  # kfbmisc::theme_kyle(base_size = 16) +
  theme(
    # Left-align title
    plot.title.position = "plot",
    # Vertically align guides
    legend.box = "vertical",
    # Left-align legend
    legend.location = "plot",
    legend.position = "top",
    legend.margin = margin(0, 0, 4, 0),
    legend.justification = c(0, 1),
    legend.background = element_rect(colour = "black")
  )



