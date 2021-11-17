
#' Preview html string in RStudio viewer pane
#'
#' @description
#'
#' This function lets you preview html in the Viewer tab of RStudio. This
#' is helpful when creating things with custom css using htmltools
#' (or modifying gt tables)
#'
#' @param html A valid html string. Can be a `character`, `html` object from
#'   `gt::html`, or `shiny.tag` from `htmltools`.
#' @export
#'
#' @section Examples:
#'
#' ```{r results = "hold", comment = "#>"}
#' library(fontawesome)
#' library(htmltools)
#' more_or_less_css <- css(
#'     font.size = "13px",
#'     width = "80px",
#'     font.family = "arial",
#'     display = "flex",
#'     flex.direction = "column",
#'     justify.content = "center",
#'     text.align = "center",
#'     padding.right = "5px",
#'     color = "#999"
#' )
#'
#' more_or_less <- div(
#'     style = more_or_less_css,
#'     div(
#'         style = css(padding.bottom = "20px"),
#'         fontawesome::fa("angle-double-up"),
#'         p("Increasing")
#'     ),
#'     div(
#'         style = css(padding.top = "40px"),
#'         p("Decreasing"),
#'         fontawesome::fa("angle-double-down")
#'     )
#' )
#'
#' # preview_html(more_or_less)
#' ```
#'
#'
preview_html <- function(html) {

	# Convert to character
	if(inherits(html, "html") | inherits(html, "shiny.tag")) {
		html = as.character(html)
	}

	# Confirm html is character
	if(!is.character(html)) {
		stop("html has to be a character")
	}

	if(interactive()) {
		htmlFile <- tempfile("index", fileext = ".html")
		writeLines(html, con = htmlFile)

		rstudioapi::viewer(htmlFile)
	} else{
		stop("This function can only be run in an interactive session")
	}
}


