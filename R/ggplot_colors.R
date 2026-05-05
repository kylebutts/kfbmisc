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
#' They are accessible: https://projects.susielu.com/viz-palette?colors=[%22#002c55%22,%22#b3114b%22,%22#5c4cbf%22,%22#158ea6%22,%22#fb7185%22,%22#77c669%22,%22#ffc517%22]&backgroundColor=%22white%22&fontColor=%22black%22&mode=%22achromatopsia%22
#'
#' The order of the colors go from dark to light
#'
#' @export
kyle_colors <- c(
  # "navy" = "#002C55",
  "teal" = "#1A505A",
  "magenta" = "#B3114B",
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
#' @examples
#' kyle_color()
#' kyle_color("magenta", "blue")
#' kyle_color(c("magenta", "blue"))
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
get_kyle_palette <- function(palette = 7, reverse = FALSE, ...) {
  p <- kyle_color(
    "teal",
    "blue",
    "yellow",
    "magenta",
    "purple",
    "rose",
    "green"
  )
  if (reverse) {
    p <- rev(p)
  }
  grDevices::colorRampPalette(p, ...)
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
    if (is.null(palette)) {
      palette <- 7
    }
    p <- get_kyle_palette(palette = palette, reverse = reverse)
    ggplot2::discrete_scale(
      "color",
      "kyle_palette",
      palette = p,
      ...
    )
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
    ggplot2::discrete_scale(
      "fill",
      "kyle_palette",
      palette = p,
      ...
    )
  } else {
    ggplot2::scale_fill_viridis_c()
  }
}
