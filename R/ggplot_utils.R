#' Preview output from \code{ggsave()}
#'
#' Displays a preview of what a ggplot-based plot looks like after being saved
#' with \code{\link{ggsave}}. Avoids the hassle of exporting a plot, switching
#' to your system file explorer, checking the output, and returning to R to make
#' more adjustments.
#'
#' The heavy lifting here came from
#' \href{https://twitter.com/tjmahr/status/1083094031826124800?s=12}{TJ Mahr}. I
#' added the Cairo option because the Cairo graphics library can (1) properly
#' embed fonts in PDFs, and (2) create PNGs with the correct DPI and dimensions.
#' See
#' \href{https://www.andrewheiss.com/blog/2017/09/27/working-with-r-cairo-graphics-custom-fonts-and-ggplot/}{this
#' blog post} for more details.
#'
#' @param device name of device to be used in \code{\link{ggsave}} (defaults to
#'   "png")
#' @param ... additional options passed to \code{\link{ggsave}}
#' @import ggplot2
#' @export
#' @examples
#'
#' library(ggplot2)
#'
#' # Generic example plot
#' plot1 <- ggplot(mtcars, aes(x = mpg, y = wt, color = factor(cyl))) +
#'   geom_point()
#'
#' # Preview with specific dimensions
#' if (interactive()) ggpreview(plot1, width = 7, height = 4)
#'
#' # Preview as PDF
#' if (interactive()) ggpreview(plot1, device = "pdf", width = 7, height = 4)
#'
ggpreview <- function(..., device = "png") {
  fname <- tempfile(fileext = paste0(".", device))

  ggplot2::ggsave(filename = fname, device = device, ...)

  system2("open", fname)
  invisible(NULL)
}


#' Create image grob from png and output the width and height
#'
#' This takes a png file and converts it to a grob. This is useful in
#' combination with `ggplot2::annotation_custom()`
#'
#' @param source name of image to read
#'
#' @export
#'
#' @examples
#'
#' library(ggplot2)
#'
#' img <- png_to_grob(system.file("img", "Rlogo.png", package = "png"))
#'
#' ggplot(mtcars, aes(x = mpg, y = wt, color = factor(cyl))) +
#'   geom_point() +
#'   annotation_custom(img, xmin = 25, xmax = 30, ymin = 4, ymax = 5)
png_to_grob <- function(source) {
  temp <- png::readPNG(source)
  message("Image has width = {dim(temp)[2]}px and height = {dim(temp)[1]}px")

  temp <- grid::rasterGrob(temp, interpolate = TRUE)
  return(temp)
}
