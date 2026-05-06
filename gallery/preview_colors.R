# %%
ex_scatter_plot <- function(colors = NULL, n_groups = 4L) {
  data(mpg, package = "ggplot2")
  mpg$class <- mpg$class |>
    stringr::str_to_title() |>
    stringr::str_replace("2seater", "2 Seater") |>
    stringr::str_replace("Suv", "SUV")

  mpg <- mpg |>
    filter(class %in% (mpg$class |> unique() |> _[1:n_groups]))

  color_scale <- if (is.null(colors)) {
    NULL
  } else {
    scale_color_manual(
      values = unname(colors),
      guide = guide_legend(nrow = 1 + ((n_groups - 1) %/% 4))
    )
  }

  ggplot(data = mpg) +
    geom_point(mapping = aes(x = displ, y = hwy, color = class)) +
    labs(
      x = "Engine size in litres",
      y = "Miles per gallon",
      color = NULL
    ) +
    color_scale +
    theme_kyle(legend = "bottom")
}

# %%
tikzsave(
  filename = here("gallery/figures/kyle_colors_ex.pdf"),
  plot = ex_scatter_plot(kyle_colors, 5L),
  width = 8,
  height = 5.5,
  create_png = TRUE
)


# %%
make_tailwind_swatch <- function() {
  hues <- unique(sub("-\\d+$", "", names(tailwind_colors)))
  shades <- sort(as.numeric(unique(sub("^.*-", "", names(tailwind_colors)))))

  df <- expand.grid(
    shade = shades,
    hue = factor(hues, levels = rev(hues)),
    stringsAsFactors = FALSE
  )
  df$color <- tailwind_colors[paste0(df$hue, "-", df$shade)]
  df$pos <- rep(1:length(shades), length(hues))

  ggplot(df, aes(x = pos, y = hue, fill = color)) +
    geom_tile() +
    scale_fill_identity() +
    # coord_fixed(ratio = 1) +
    scale_x_continuous(
      breaks = 1:length(shades),
      labels = shades,
    ) +
    labs(x = "Shade", y = "Hue") +
    theme_kyle()
}

tikzsave(
  filename = here("gallery/figures/tailwind_colors.pdf"),
  plot = make_tailwind_swatch(),
  width = 8,
  height = 5.5,
  create_png = TRUE
)


# %%
make_swatch <- function(colors) {
  color_df <- data.frame(colors = colors)
  color_df$y <- seq_len(nrow(color_df))

  # Plot the swatch of colors
  ggplot(color_df, aes(x = 1, y = y, fill = colors)) +
    geom_tile() +
    scale_fill_identity() +
    coord_fixed(ratio = 0.2) + # Adjusts the size of each swatch
    theme_void() + # Removes axis and grid lines
    theme(legend.position = "none") # Removes the legend
}
