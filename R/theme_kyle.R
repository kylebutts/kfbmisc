#' Custom ggplot theme
#'
#' @param title_pos either "center" or "left" to align title and subtitle center of left respectively. Default is "center".
#' @param axis_title_pos either "center" or "left" to align title and subtitle center or left/top respectively. Default is "left".
#' @param slides if TRUE, use background #ECECEC. if FALSE, transparent background. Default is False.
#' @param has_subtitle Adjusts margins if there is a subtitle. Default is False.
#' @param base_size The base_size of the font. Title, axis text, etc. all adjust to base_size. Default is 14.
#'
#' @examples
#'
#' library(ggplot)
#' ggplot(mtcars) +
#'    geom_point(aes(x = mpg, y = hp)) +
#'    theme_kyle(base_size = 18)
#'
#' @export
theme_kyle <- function(title_pos = "center", axis_title_pos = "left", slides = FALSE, has_subtitle = FALSE, base_size = 14, ...) {

	title_hjust <- switch(title_pos, "center" = 0.5, "left" = 0)
	axis_title_hjust_y <- switch(axis_title_pos, "center" = 0.5, "left" = 1.0)
	axis_title_hjust_x <- switch(axis_title_pos, "center" = 0.5, "left" = 0.0)
	plot_bg = dplyr::if_else(slides, "#ECECEC", "transparent")
	plot_grid = dplyr::if_else(slides, "grey85", "grey92")
	title_margin = dplyr::if_else(has_subtitle, "4", "16")

	ggplot2::theme_bw(
		base_size = base_size,
		base_family = "fira_sans"
	) +
	ggplot2::theme(
		## Title and Subtitle --------------------------------------------------
		plot.title = ggplot2::element_text(
			# Font
			family = "merriweather", face = "bold", size = ggplot2::rel(1.285),
			colour = "#454545",
			# Center title
			hjust = title_hjust,
			# Margins
			margin = ggplot2::margin(b = title_margin, unit = "pt")
		),
		plot.subtitle = ggplot2::element_text(
			# Font
			family = "merriweather", face = "italic", size = ggplot2::rel(.86),
			colour = "#454545",
			# Center subtitle
			hjust = title_hjust,
			# Margins
			margin = ggplot2::margin(b = 16, unit = "pt")
		),
		plot.title.position = "plot",

		## Caption -------------------------------------------------------------
		plot.caption = ggplot2::element_text(
			# Font
			size = ggplot2::rel(0.72), colour = "#454545",
			# Right-align caption
			hjust = 1,
			# Margins
			margin = ggplot2::margin(t = 20)
		),
		plot.caption.position = "plot",

		## Axis ----------------------------------------------------------------
		# Axis title
		axis.title = ggplot2::element_text(
			# Font
			size = ggplot2::rel(.86), colour = "#454545", face = "italic"
		),
		# Axis Title x/y
		axis.title.y = ggplot2::element_text(
			# Right-align y axis title
			hjust = axis_title_hjust_y,
			# Margins
			margin = ggplot2::margin(r = 10)
		),
		axis.title.x = ggplot2::element_text(
			# Left-align x axis title
			hjust = axis_title_hjust_x,
			# Margins
			margin = ggplot2::margin(t = 10)
		),
		# Axis labels
		axis.text = ggplot2::element_text(
			# Font
			size = ggplot2::rel(.72), colour = "#212121"
		),
		# Axis Lines
		axis.line = ggplot2::element_line(
			colour = "grey40"
		),
		panel.grid = ggplot2::element_line(
			colour = plot_grid
		),


		## Legend -------------------------------------------------------------
		# Legend title
		legend.title = ggplot2::element_text(
			# Font
			size = ggplot2::rel(.86), colour = "#454545"
		),
		# Legend labels
		legend.text = ggplot2::element_text(
			# Font
			size = ggplot2::rel(.72), colour = "#454545"
		),
		legend.background = ggplot2::element_rect(
			# No Background Colour
			fill = "transparent", colour = NA
		),
		legend.key = ggplot2::element_rect(
			# No Background Colour
			fill = "transparent", colour = NA
		),


		## Facet Wrap ----------------------------------------------------------
		strip.text = ggplot2::element_text(
			# Font
			size = ggplot2::rel(.86), colour = "#454545",
			# Margin
			margin = ggplot2::margin(t= 10, b= 10)
		),
		strip.background = ggplot2::element_rect(
			# No Background Colour
			fill = "transparent", colour = NA
		),

		## Panel ---------------------------------------------------------------
		panel.background = ggplot2::element_rect(
			# No Background Colour
			fill = plot_bg, colour = NA
		),
		panel.border = ggplot2::element_rect(
			# No Background Colour
			colour = NA
		),
		panel.spacing = grid::unit(8, "points"),
		## Plot ----------------------------------------------------------------
		plot.background = ggplot2::element_rect(
			# No Background Colour
			fill = plot_bg, colour = NA
		),
		plot.margin = ggplot2::margin(16, 16, 16, 16, unit = "pt")
	) +
	## Additional options passed by user ---------------------------------------
	ggplot2::theme(
		...
	)
}
