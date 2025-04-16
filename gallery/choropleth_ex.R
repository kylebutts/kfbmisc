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
