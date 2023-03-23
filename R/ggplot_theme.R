#' Custom ggplot2 theme
#'
#' @param base_size The base_size of the font. Title, axis text, etc. all adjust to base_size. Default is 14.
#' @param ... Additoinal options passed to `ggplot2::theme`
#'
#' @examples
#'
#' ggplot2::ggplot(mtcars) +
#'   ggplot2::geom_point(ggplot2::aes(x = mpg, y = hp)) +
#'   theme_kyle(base_size = 18)
#' 
#' ggplot2::ggplot(mtcars) + 
#'   ggplot2::geom_point(ggplot2::aes(x = mpg, y = hp)) + 
#'   ggplot2::facet_wrap(~ cyl) +
#'   ggplot2::labs(
#'     title = "mtcars Dataset", 
#'     x = "Miles per Gallon", y = "Horsepower"
#'   ) +
#'   theme_kyle(base_size = 18)
#'
#' @export
theme_kyle <- function(base_size = 14, ...) {
	
	ggplot2::theme_bw(
		base_size = base_size
	) + 
		theme(
			## Title and Subtitle --------------------------------------------------
			plot.title = ggplot2::element_text(
				# Font
				face = "bold", size = ggplot2::rel(1.285), 
				colour = "#454545", 
				# Center title
				hjust = 0,
				# Margins
				margin = ggplot2::margin(b = 8, unit = "pt")
			),
			plot.subtitle = ggplot2::element_text(
				# Font
				family = "merriweather", face = "italic", size = ggplot2::rel(.86), 
				colour = "#454545", 
				# Center subtitle 
				hjust = 0,
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
				size = ggplot2::rel(1.285), colour = "#454545"
			),
			# Axis Title x/y
			axis.title.y = ggplot2::element_text(
				# Right-align y axis title
				hjust = 0.5,
				# Margins
				margin = ggplot2::margin(r = 10)
			),
			axis.title.x = ggplot2::element_text(
				# Left-align x axis title
				hjust = 0.5,
				# Margins
				margin = ggplot2::margin(t = 10)
			),
			# Axis labels
			axis.text = ggplot2::element_text(
				# Font
				size = ggplot2::rel(1), colour = "#212121"
			),
			# Axis Lines
			axis.line = ggplot2::element_line(
				colour = "grey40"
			),
      axis.ticks = ggplot2::element_line(
				colour = "grey40"
			),
			panel.grid = ggplot2::element_line(
				colour = "grey92"
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
			
			## Facet Wrap ----------------------------------------------------------
			strip.text = ggplot2::element_text(
				# Font 
				size = ggplot2::rel(.86), colour = "#454545", 
				# Margin							   
				margin = ggplot2::margin(t = 10, b= 10)
			),
			strip.background = ggplot2::element_rect(
        fill = "white", 
        colour = "grey40", linewidth = 2,
			),
      panel.margin = grid::unit(2, "lines"),
			
			## Panel ---------------------------------------------------------------
			panel.background = ggplot2::element_rect(
        colour = NA
			),
			panel.border = ggplot2::element_rect(
				colour = NA
			),
			panel.spacing = grid::unit(8, "points"),
			
			## Plot ----------------------------------------------------------------
			plot.background = ggplot2::element_rect(
        colour = NA
			),
			plot.margin = ggplot2::margin(16, 16, 16, 16, unit = "pt")
		) +
		## Additional options passed by user ---------------------------------------
	theme(
		...
	)
}




#' Remove axis for maps
#'
#' Add this to `theme_kyle` to produce maps without axis lines.
#'
#' @export
theme_map <- function() {
    ## Additional options passed by user ---------------------------------------
    ggplot2::theme(
    		axis.text = ggplot2::element_blank(),
    		axis.ticks = ggplot2::element_blank(),
    		panel.background = ggplot2::element_rect(fill = "white"),
    		panel.grid.major = ggplot2::element_blank(),
    		axis.line = ggplot2::element_blank(),
    		axis.title = ggplot2::element_blank()
    )
}
