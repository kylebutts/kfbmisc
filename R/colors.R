# Other color palettes I like
pilot_colors <- c(
  "navy" = "#204466",
  "blue" = "#249db5",
  "brown" = "#b84818",
  "green" = "#30c788",
  "yellow" = "#ffc517",
  "purple" = "#9956db",
  "orange" = "#f28100"
)
ed_rubin_colors <- c(
  "red_pink" = "#e64173",
  "turquoise" = "#20B2AA",
  "orange" = "#FFA500",
  "red" = "#fb6107",
  "blue" = "#3b3b9a",
  "green" = "#8bb174",
  "purple" = "#6A5ACD"
)

# Old: https://projects.susielu.com/viz-palette?colors=[%22#002c55%22,%22#b3114b%22,%22#5c4cbf%22,%22#158ea6%22,%22#f28100%22,%22#77c669%22,%22#ffc517%22]&backgroundColor=%22white%22&fontColor=%22black%22&mode=%22normal%22

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

ex_scatter_plot <- function(colors) {
  data(mpg, package = "ggplot2")
  mpg$class <- mpg$class |>
    stringr::str_to_title() |>
    stringr::str_replace("2seater", "2 Seater") |>
    stringr::str_replace("Suv", "SUV")

  ggplot(data = mpg) +
    geom_point(mapping = aes(x = displ, y = hwy, color = class)) +
    labs(
      x = "Engine size in litres",
      y = "Miles per gallon",
      color = NULL
    ) +
    scale_color_manual(
      values = unname(colors),
      guide = guide_legend(nrow = 2)
    ) +
    theme_kyle(base_size = 14) +
    theme(
      legend.position = "bottom",
      legend.justification = c(0.5, 0)
    )
}


#' Set of colors for graphics that I like
#'
#' They are accessible: https://projects.susielu.com/viz-palette?colors=[%22#002c55%22,%22#b3114b%22,%22#5c4cbf%22,%22#158ea6%22,%22#fb7185%22,%22#77c669%22,%22#ffc517%22]&backgroundColor=%22white%22&fontColor=%22black%22&mode=%22achromatopsia%22
#'
#' The order of the colors go from dark to light
#'
#' @export
kyle_colors <- c(
  "navy" = "#002C55",
  "magenta" = "#B3114B",
  "purple" = "#5C4CBF",
  # "blue" = "#158EA6",
  "blue" = "#0188AC",
  "green" = "#2DB25F",
  "rose" = "#FB7185",
  "yellow" = "#ffc517"
)

#' Get color from my palette by name
#'
#' @param ... Character(s) of colors
#'
#' @return Vector of colors. If color is null, returns the full palette.
#'   Otherwise, the vector is the corresponding colors
#'
#' @examples
#' kyle_color()
#' kyle_color("magenta", "blue")
#'
#' @export
kyle_color <- function(...) {
  colors <- c(...)
  if (is.null(colors)) {
    return(kyle_colors)
  }
  return(unname(kyle_colors[colors]))
}


# get colors
get_kyle_palette <- function(palette = "seven", reverse = FALSE, ...) {
  kyle_palettes <- list(
    two = kyle_color(
      "navy",
      "blue"
    ),
    three = kyle_color(
      "navy",
      "blue",
      "yellow"
    ),
    four = kyle_color(
      "navy",
      "blue",
      "yellow",
      "magenta"
    ),
    five = kyle_color(
      "navy",
      "blue",
      "yellow",
      "magenta",
      "purple"
    ),
    six = kyle_color(
      "navy",
      "blue",
      "yellow",
      "magenta",
      "purple",
      "rose"
    ),
    seven = kyle_color(
      "navy",
      "blue",
      "yellow",
      "magenta",
      "purple",
      "rose",
      "green"
    )
  )

  p <- kyle_palettes[[palette]]
  if (reverse) p <- rev(p)
  grDevices::colorRampPalette(p, ...)
}

#' Color scale for kyle colors
#'
#' @param palette Name of palette in \code{kyle_palettes}.
#' @param discrete Boolean to indicate if color aesthetic is discrete.
#' @param reverse Boolean to indicate whether palette should be reversed.
#' @param ... Additional arguments passed to \code{discrete_scale} or
#'   \code{scale_color_gradientn}, depending on the value of \code{discrete}.
#' @export
scale_color_kyle <- function(
  palette = "seven",
  discrete = TRUE,
  reverse = FALSE,
  ...
) {
  p <- get_kyle_palette(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale(
      "color",
      paste0("kyle_", palette),
      palette = p,
      ...
    )
  } else {
    ggplot2::scale_color_gradientn(colors = p(256), ...)
  }
}

#' Fill scale for kyle colors
#'
#' @param palette Name of palette in \code{kyle_palettes}.
#' @param discrete Boolean to indicate if color aesthetic is discrete.
#' @param reverse Boolean to indicate whether palette should be reversed.
#' @param ... Additional arguments passed to \code{discrete_scale} or
#'   \code{scale_color_gradientn}, depending on the value of \code{discrete}.
#' @export
scale_fill_kyle <- function(
  palette = "seven",
  discrete = TRUE,
  reverse = FALSE,
  ...
) {
  p <- get_kyle_palette(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale(
      "fill",
      paste0("kyle_", palette),
      palette = p,
      ...
    )
  } else {
    ggplot2::scale_fill_gradientn(colors = p(256), ...)
  }
}
