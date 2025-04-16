library(tidyverse)
library(kfbmisc)
library(sf)
nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

(map <- ggplot(nc) +
  geom_sf(aes(fill = BIR74)) +
  scale_fill_fermenter(
    direction = 1,
    palette = 3,
    guide = guide_colorbar(direction = "horizontal")
  ) +
  labs(fill = "1974 Births") +
  theme_kyle(map = TRUE) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.1, 0.07),
    legend.justification = c(0, 0),
    legend.title.position = "top",
    legend.key.size = unit(5, "lines"),
    legend.key.height = unit(16, "pt")
  ))


ggplot() +
  geom_point(aes(x = mpg, y = hp, color = hp), mtcars) +
  theme(
    legend.title.position = "top",
    legend.key.size = unit(2, "lines"),
    legend.key.height = unit(12, "pt")
  )

# %%
library(ggplot2)
ggplot() +
  geom_point(aes(x = mpg, y = hp, color = hp), mtcars) +
  theme(
    legend.title.position = "top",
    legend.key.width = unit(12, "pt"),
    legend.key.height = unit(12, "pt")
  )

# Height does not work
ggplot() +
  geom_point(aes(x = mpg, y = hp, color = hp), mtcars) +
  theme(
    legend.title.position = "top",
    legend.key.width = unit(12, "pt"),
    legend.key.height = unit(48, "pt")
  )

# Width works
ggplot() +
  geom_point(aes(x = mpg, y = hp, color = hp), mtcars) +
  theme(
    legend.title.position = "top",
    legend.key.width = unit(48, "pt"),
    legend.key.height = unit(12, "pt")
  )

# size changes height
ggplot() +
  geom_point(aes(x = mpg, y = hp, color = hp), mtcars) +
  theme(
    legend.title.position = "top",
    legend.key.size = unit(48, "pt"),
    legend.key.width = unit(12, "pt"),
  )

# can override with width
# can not override with height
ggplot() +
  geom_point(aes(x = mpg, y = hp, color = hp), mtcars) +
  theme(
    legend.title.position = "top",
    legend.key.size = unit(48, "pt"),
    legend.key.width = unit(12, "pt"),
    legend.key.height = unit(12, "pt")
  )

# %%
# when using `legend.position = "bottom"`, width now causes the problems
ggplot() +
  geom_point(aes(x = mpg, y = hp, color = hp), mtcars) +
  theme(
    legend.position = "bottom",
    legend.title.position = "top",
    legend.key.width = unit(12, "pt"),
    legend.key.height = unit(12, "pt")
  )

# Height works
ggplot() +
  geom_point(aes(x = mpg, y = hp, color = hp), mtcars) +
  theme(
    legend.position = "bottom",
    legend.title.position = "top",
    legend.key.width = unit(12, "pt"),
    legend.key.height = unit(48, "pt")
  )

# Width does not work
ggplot() +
  geom_point(aes(x = mpg, y = hp, color = hp), mtcars) +
  theme(
    legend.position = "bottom",
    legend.title.position = "top",
    legend.key.width = unit(48, "pt"),
    legend.key.height = unit(12, "pt")
  )

# size changes width
ggplot() +
  geom_point(aes(x = mpg, y = hp, color = hp), mtcars) +
  theme(
    legend.position = "bottom",
    legend.title.position = "top",
    legend.key.size = unit(48, "pt"),
    legend.key.height = unit(12, "pt"),
  )

# can override with height
# can not override with width
ggplot() +
  geom_point(aes(x = mpg, y = hp, color = hp), mtcars) +
  theme(
    legend.position = "bottom",
    legend.title.position = "top",
    legend.key.size = unit(48, "pt"),
    legend.key.width = unit(12, "pt"),
    legend.key.height = unit(12, "pt")
  )
