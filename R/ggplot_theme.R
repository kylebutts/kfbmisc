# TODO: Adapt from `theme_bw()` base_family = "", base_line_size = base_size/22, base_rect_size = base_size/22,

# Needed b/c ggplot2::element_* acts weird with `ifelse`
grepl_ifelse <- function(pattern, x, yes, no) {
  if (grepl(pattern, x)) {
    return(yes)
  } else {
    return(no)
  }
}

#' Custom ggplot2 theme
#'
#' @param base_size The base_size of the font. Title, axis text, etc. all adjust to base_size. Default is 14.
#' @param axes A string indicating which axes should have lines and ticks.
#'   Specify which axes to show by including the matching characters in the
#'   string: "t" for top, "r" for right, "b" for bottom, "l" for left. You will
#'   need to ensure this argument is consistent with the axes settings in your
#'   plot for the lines and ticks to be displayed. The default is an empty
#'   string, meaning ticks and lines for the bottom and left axes are shown by
#'   default.
#' @param grid A string indicating which gridlines should be shown. Specify
#'   the gridlines to show by including the matching characters in the string:
#'   "h" for horizontal, "v" for vertical. The default is "hv",
#'   meaning both gridlines are shown by default.
#' @param grid_minor A string indicating which gridlines should be shown. Specify
#'   the gridlines to show by including the matching characters in the string:
#'   "h" for horizontal, "v" for vertical. The default takes the value of
#'   `grid` unless overriden.
#' @param legend Either "top", "bottom", or "right".
#' @param scale logical.
#' @param map Logical. If true, clear axes and plot for maps.
#'    Could alternatively  add `theme_map` to plot.
#' @param ... Additional options passed to `ggplot2::theme`
#'
#' @examples
#'
#' ggplot2::ggplot(mtcars) +
#'   ggplot2::geom_point(ggplot2::aes(x = mpg, y = hp)) +
#'   theme_kyle(base_size = 18)
#'
#' ggplot2::ggplot(mtcars) +
#'   ggplot2::geom_point(ggplot2::aes(x = mpg, y = hp)) +
#'   ggplot2::facet_wrap(~cyl) +
#'   ggplot2::labs(
#'     title = "mtcars Dataset",
#'     x = "Miles per Gallon", y = "Horsepower"
#'   ) +
#'   theme_kyle(base_size = 18)
#'
#' @importFrom ggplot2 '%+replace%'
#' @return A (complete) ggplot2 theme
#'
#' @export
theme_kyle <- function(
  base_size = 12,
  axes = "bl",
  grid = "hv",
  grid_minor = "",
  legend = "right",
  map = FALSE,
  scale = 1.125,
  ...
) {
  # Fluid scale: https://utopia.fyi/type/
  SCALE <- scale

  grid_line_color <- tailwind_color("zinc-200")
  grid_line <- ggplot2::element_line(
    color = grid_line_color,
    linewidth = 0.35,
    linetype = "solid"
  )

  grid_minor_line_color <- tailwind_color("zinc-200")
  grid_minor_line <- ggplot2::element_line(
    color = grid_minor_line_color,
    linewidth = 0.2,
    linetype = "solid"
  )

  axis_line_color <- tailwind_color("zinc-500")
  axis_line <- ggplot2::element_line(
    color = axis_line_color,
    linewidth = 0.3,
    linetype = "solid",
    inherit.blank = FALSE
  )

  theme_kyle <-
    ggplot2::theme_bw(
      base_size = base_size
    ) %+replace%
    ggplot2::theme(
      ## Plot
      plot.background = ggplot2::element_rect(
        color = "white",
      ),
      plot.margin = ggplot2::margin(16, 16, 16, 16, unit = "pt"),

      ## Panel
      panel.background = ggplot2::element_rect(
        color = "white",
        fill = "white"
      ),
      panel.border = ggplot2::element_blank(),
      panel.spacing = grid::unit(1.5, "lines"),

      ## Title & subtitle
      plot.title = ggplot2::element_text(
        size = ggplot2::rel(SCALE^2),
        color = tailwind_color("zinc-900"),
        hjust = 0,
        margin = ggplot2::margin(b = 8, unit = "pt")
      ),
      plot.subtitle = ggplot2::element_text(
        size = ggplot2::rel(SCALE),
        color = tailwind_color("zinc-500"),
        hjust = 0,
        margin = ggplot2::margin(b = 16, unit = "pt")
      ),
      plot.title.position = "plot",

      ## Caption
      plot.caption = ggplot2::element_text(
        size = ggplot2::rel(1 / SCALE),
        color = tailwind_color("zinc-500"),
        hjust = 1,
        margin = ggplot2::margin(t = 20)
      ),
      plot.caption.position = "plot",

      ## Axes
      axis.title = ggplot2::element_text(
        size = ggplot2::rel(1),
        color = tailwind_color("zinc-800")
      ),
      axis.title.y = ggplot2::element_text(
        hjust = 0.5,
        angle = 90,
        margin = ggplot2::margin(r = 10)
      ),
      axis.title.x = ggplot2::element_text(
        hjust = 0.5,
        margin = ggplot2::margin(t = 10)
      ),
      axis.text = ggplot2::element_text(
        size = ggplot2::rel(1 / SCALE),
        color = tailwind_color("zinc-700")
      ),

      # Axes
      axis.ticks = axis_line,
      axis.line = axis_line,
      axis.line.x.top = grepl_ifelse(
        "t",
        axes,
        axis_line,
        ggplot2::element_blank()
      ),
      axis.ticks.x.top = grepl_ifelse(
        "t",
        axes,
        axis_line,
        ggplot2::element_blank()
      ),
      axis.line.y.right = grepl_ifelse(
        "r",
        axes,
        axis_line,
        ggplot2::element_blank()
      ),
      axis.ticks.y.right = grepl_ifelse(
        "r",
        axes,
        axis_line,
        ggplot2::element_blank()
      ),
      axis.line.x.bottom = grepl_ifelse(
        "b",
        axes,
        axis_line,
        ggplot2::element_blank()
      ),
      axis.ticks.x.bottom = grepl_ifelse(
        "b",
        axes,
        axis_line,
        ggplot2::element_blank()
      ),
      axis.line.y.left = grepl_ifelse(
        "l",
        axes,
        axis_line,
        ggplot2::element_blank()
      ),
      axis.ticks.y.left = grepl_ifelse(
        "l",
        axes,
        axis_line,
        ggplot2::element_blank()
      ),

      ## Legend
      legend.background = ggplot2::element_rect(
        color = "white"
      ),
      legend.key = ggplot2::element_rect(
        color = "white",
        fill = "white"
      ),
      legend.title = ggplot2::element_text(
        size = ggplot2::rel(1),
        color = tailwind_color("zinc-700")
      ),
      legend.text = ggplot2::element_text(
        size = ggplot2::rel(1 / SCALE),
        color = tailwind_color("zinc-700")
      ),

      ## Facet Wrap
      strip.background = ggplot2::element_rect(
        color = "white",
        fill = "white"
      ),
      strip.text = ggplot2::element_text(
        size = ggplot2::rel(1 / SCALE),
        color = tailwind_color("zinc-800"),
        margin = ggplot2::margin(t = 0, b = 8)
      ),

      ## Gridlines
      # Default to none, edited below
      panel.grid.major = grid_line,
      panel.grid.minor = grid_line,
      panel.grid.major.y = grepl_ifelse(
        "h",
        grid,
        grid_line,
        ggplot2::element_blank()
      ),
      panel.grid.minor.y = grepl_ifelse(
        "h",
        grid_minor,
        grid_line,
        ggplot2::element_blank()
      ),
      panel.grid.major.x = grepl_ifelse(
        "v",
        grid,
        grid_line,
        ggplot2::element_blank()
      ),
      panel.grid.minor.x = grepl_ifelse(
        "v",
        grid_minor,
        grid_line,
        ggplot2::element_blank()
      ),

      # https://ggplot2-book.org/extensions#complete-themes
      complete = TRUE
    )

  if (map == TRUE) {
    theme_kyle <- theme_kyle %+replace%
      ggplot2::theme(
        panel.background = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.ticks.x.top = ggplot2::element_blank(),
        axis.ticks.x.bottom = ggplot2::element_blank(),
        axis.ticks.y.left = ggplot2::element_blank(),
        axis.ticks.y.right = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        axis.line.x.top = ggplot2::element_blank(),
        axis.line.x.bottom = ggplot2::element_blank(),
        axis.line.y.left = ggplot2::element_blank(),
        axis.line.y.right = ggplot2::element_blank(),
        complete = TRUE
      )
  }

  if (legend == "top") {
    theme_kyle <- theme_kyle %+replace%
      ggplot2::theme(
        legend.position = "top",
        legend.margin = margin(0, 0, 5, 0),
        legend.justification = c(0, 1),
        legend.location = "plot",
        legend.key.spacing.x = unit(12, "pt"),
        complete = TRUE
      )
  } else if (legend == "bottom") {
    theme_kyle <- theme_kyle %+replace%
      ggplot2::theme(
        legend.position = "bottom",
        legend.title.position = "top",
        legend.margin = margin(5, 0, 0, 0),
        legend.justification = "center",
        legend.location = "plot",
        legend.key.spacing.x = unit(12, "pt"),
        complete = TRUE
      )
  }

  # Additional options passed by user
  theme_kyle <- theme_kyle %+replace% ggplot2::theme(...)

  return(theme_kyle)
}

#' Remove axis for maps
#'
#' Add this to `theme_kyle` to produce maps without axis lines.
#'
#' @export
theme_map <- function() {
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "white"),
    panel.grid.major = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.ticks.x.top = ggplot2::element_blank(),
    axis.ticks.x.bottom = ggplot2::element_blank(),
    axis.ticks.y.left = ggplot2::element_blank(),
    axis.ticks.y.right = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    axis.line.x.top = ggplot2::element_blank(),
    axis.line.x.bottom = ggplot2::element_blank(),
    axis.line.y.left = ggplot2::element_blank(),
    axis.line.y.right = ggplot2::element_blank(),
    complete = TRUE
  )
}
