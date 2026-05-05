ex_scatter_plot <- function(colors = NULL) {
  data(mpg, package = "ggplot2")
  mpg$class <- mpg$class |>
    stringr::str_to_title() |>
    stringr::str_replace("2seater", "2 Seater") |>
    stringr::str_replace("Suv", "SUV")

  color_scale <- if (is.null(colors)) {
    NULL
  } else {
    scale_color_manual(
      values = unname(colors),
      guide = guide_legend(nrow = 2)
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
    coord_fixed(ratio = 1) +
    scale_x_continuous(
      breaks = 1:length(shades),
      labels = shades,
    ) +
    labs(x = "Shade", y = "Hue") +
    theme_kyle()
}
