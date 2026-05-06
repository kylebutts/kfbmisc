# Other color palettes I like
# Can access with `kfbmisc:::ed_rubin_colors` and `kfbmisc:::pilot_colors`
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
paulgp_colors <- c(
  "green" = "#199D76",
  "orange" = "#D96003",
  "purple" = "#7570B3"
)


#' Set of colors for graphics that I like
#'
#' They are accessible:
#' https://projects.susielu.com/viz-palette?colors=%5B%22%2348000f%22%2C%22%23003a40%22%2C%22%2351419f%22%2C%22%23158ea6%22%2C%22%23fb7185%22%2C%22%2377c669%22%2C%22%23ffc517%22%5D&backgroundColor=%22white%22&fontColor=%22black%22&mode=%22none%22
#'
#' The order of the colors are ordered for adding more colors to a chart that are visually distinct
#'
#' From dark to light: crimson, teal, magenta, purple, blue, rose, green, yellow
#'
#' @export
kyle_colors <- c(
  "magenta" = "#B3114B",
  "yellow" = "#ffc517",
  "blue" = "#158ea6",
  "green" = "#77c669",
  "crimson" = "#48000f",
  "purple" = "#51419f",
  "rose" = "#fb7185",
  "teal" = "#003a40"
)

archive_kyle_colors <- c(
  "navy" = "#002C55",
  "magenta" = "#B3114B",
  "teal" = "#1A505A",
  "purple" = "#5C4CBF",
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
get_kyle_palette <- function(reverse = FALSE, ...) {
  p <- kyle_color(
    "magenta",
    "yellow",
    "blue",
    "green",
    "crimson",
    "purple",
    "rose",
    "teal"
  )
  if (reverse) {
    p <- rev(p)
  }
  return(p)
}

#' Color scale for kyle colors
#'
#' @param discrete Boolean to indicate if color aesthetic is discrete.
#' @param reverse Boolean to indicate whether palette should be reversed.
#' @param ... Additional arguments passed to \code{discrete_scale} or
#'   \code{scale_color_viridis_c}, depending on the value of \code{discrete}.
#' @export
scale_color_kyle <- function(
  discrete = TRUE,
  reverse = FALSE,
  ...
) {
  if (discrete) {
    p <- get_kyle_palette(reverse = reverse)
    ggplot2::scale_color_manual(values = p)
  } else {
    ggplot2::scale_color_viridis_c()
  }
}

#' Fill scale for kyle colors
#'
#' @param discrete Boolean to indicate if color aesthetic is discrete.
#' @param reverse Boolean to indicate whether palette should be reversed.
#' @param ... Additional arguments passed to \code{discrete_scale} or
#'   \code{scale_fill_viridis_c}, depending on the value of \code{discrete}.
#' @export
scale_fill_kyle <- function(
  discrete = TRUE,
  reverse = FALSE,
  ...
) {
  if (discrete) {
    p <- get_kyle_palette(reverse = reverse)
    ggplot2::scale_fill_manual(values = p)
  } else {
    ggplot2::scale_fill_viridis_c()
  }
}
