# #' Nice looking annotations using `ggtext``
# #'
# #' @param label Text to be displayed
# #' @param x x coordinate of the label
# #' @param y y coordinate of the label
# #' @param size Text size in points
# #' @param width width of textbox
# #' @param text.color color of text
# #' @param fill background color
# #' @param border.color color of border
# #' @param linewidth width of border
# #' @param label.r radius of rounded corners
# #' @param label.padding padding around the label
# #' @param hjust horizontal justification
# #' @param vjust vertical justificationw
# #'
# #' @return A ggplot layer object with a textbox annotation
# #'
# #' @export
# kyle_textbox <- function(
#   label,
#   x,
#   y,
#   size = 10,
#   width = unit(0.2, "npc"),
#   text.color = tailwind_color("zinc-800"),
#   fill = "white",
#   box.color = tailwind_color("zinc-300"),
#   box.size = unit(0.5, "pt"),
#   box.r = unit(4, "pt"),
#   box.padding = unit(c(6, 6, 6, 6), "pt"),
#   hjust = 0,
#   vjust = 1
# ) {
#   annotate(
#     "textbox",
#     label = label,
#     x = x,
#     y = y,
#     width = width,
#     size = pt_to_mm(size),
#     text.color = text.color,
#     fill = fill,
#     box.color = box.color,
#     box.r = box.r,
#     box.padding = box.padding,
#     box.size = box.size,
#     hjust = hjust,
#     vjust = vjust
#   )
# }

# #' Custom default for `label` annotation
# #'
# #' @param label Text to be displayed
# #' @param x x coordinate of the label
# #' @param y y coordinate of the label
# #' @param size Text size in points
# #' @param size.unit units for size
# #' @param text.color color of text
# #' @param fill background color
# #' @param border.color color of border
# #' @param linewidth width of border
# #' @param label.r radius of rounded corners
# #' @param label.padding padding around the label
# #' @param hjust horizontal justification
# #' @param vjust vertical justification
# #'
# #' @return A ggplot layer object with a label annotation
# #'
# #' @export
# kyle_label <- function(
#   label,
#   x,
#   y,
#   size = 10,
#   size.unit = "pt",
#   text.color = tailwind_color("zinc-800"),
#   fill = "white",
#   border.color = tailwind_color("zinc-300"),
#   linewidth = 0.5,
#   label.r = unit(4, "pt"),
#   label.padding = unit(6, "pt"),
#   hjust = 0,
#   vjust = 1
# ) {
#   annotate(
#     "label",
#     label = label,
#     x = x,
#     y = y,
#     size = size,
#     size.unit = size.unit,
#     text.color = text.color,
#     fill = fill,
#     border.color = border.color,
#     linewidth = linewidth,
#     label.r = label.r,
#     label.padding = label.padding,
#     hjust = 0,
#     vjust = 1,
#   )
# }

#' Convert a numeric vector specified in pt to mm
#' @param x Numeric vector of units specified in pt
#' @return Numeric vector of units specified in mm
#' @export
pt_to_mm <- function(x) {
  # 72.27 pt = 1 in
  # 1 in = 25.4 mm
  return(x / 72.72 * 25.4)
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
