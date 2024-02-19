# %%
#| warning: false
library(kfbmisc)
library(tidyverse)
library(here)
library(sf)

# %%
df <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv",
  show_col_type = FALSE
)

df <- df |>
  st_as_sf(
    coords = c("long", "lat"),
    crs = st_crs(4326)
  )

sample = df |>
  filter(!is.na(primary_fur_color)) |>
  slice_sample(n = 1000)

hull <- df$geometry |>
  sf::st_union() |>
  sf::st_convex_hull() |>
  sf::st_buffer(units::as_units(".2 mi"))

central_park <- sf::read_sf(here("gallery/data/central_park.geojson"))
central_park <- central_park |>
  st_intersection(hull) |>
  rmapshaper::ms_simplify(keep = 0.02)


# %%
squirrel_colors <- c(
  "Black" = "black",
  "Cinnamon" = "#5A2A14", 
  "Gray" = "#A9AAB8"
)
(map <- ggplot() +
  geom_sf(
    data = central_park,
    color = tailwind["grey-300"],
    linewidth = 0.5
  ) +
  geom_sf(
    aes(color = primary_fur_color),
    data = sample, 
    size = 0.5
  ) +
  labs(
    color = "Primary fur color"
  ) +
  scale_color_manual(
    values = squirrel_colors,
    guide = guide_legend(
      override.aes = list(size = 1)
    )
  ) +
  theme_kyle(
    base_size = 14
  ) +
  theme_map() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.15, 0.85)
  )
)

# %% 
(facet_map = map +
  facet_grid(~ primary_fur_color) +
  theme(
    legend.position = "bottom"
  )
)

# %% 
tikzsave(
  filename = here("gallery/figures/map.pdf"),
  plot = map, width = 6, height = 5,
  create_png = TRUE
)

tikzsave(
  filename = here("gallery/figures/facet_map.pdf"),
  plot = facet_map, width = 8, height = 4.5,
  create_png = TRUE
)

