

#' Custom gt theme which borrows heavily from Thomas Mock
#'
#' @examples
#'
#' library(gt)
#'
#' mtcars[1:5, ] %>%
#'     gt() %>%
#'     gt_theme_kyle %>%
#'     tab_header(title = make_gt_title("mtcars Dataset")) %>%
#'     tab_source_note(source_note = "Data from mtcars in R")
gt_theme_kyle <- function(data,...) {
	data %>%
		gt::opt_all_caps()  %>%
		gt::opt_table_font(
			font = list(
				gt::google_font("Chivo"),
				gt::default_fonts()
			)
		) %>%
		gt::tab_style(
			style = gt::cell_borders(
				sides = "bottom", color = "black", weight = gt::px(2)
			),
			locations = gt::cells_body(
				columns = gt::everything(),
				# This is a relatively sneaky way of changing the bottom border
				# Regardless of data size
				rows = nrow(data$`_data`)
			)
		)  %>%
		gt::tab_options(
			column_labels.background.color = "white",
			table.border.top.width = gt::px(3),
			table.border.top.color = "transparent",
			table.border.bottom.color = "transparent",
			table.border.bottom.width = gt::px(3),
			column_labels.border.top.width = gt::px(3),
			column_labels.border.top.color = "transparent",
			column_labels.border.bottom.width = gt::px(3),
			column_labels.border.bottom.color = "black",
			data_row.padding = gt::px(5),
			source_notes.font.size = 12,
			table.font.size = 16,
			heading.align = "left",
			...
		)
}


make_gt_title <- function(title) {
	html <- htmltools::span(style = "display: block; margin-bottom: 8px;", title)
	gt::html(as.character(html))
}
