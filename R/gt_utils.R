#' Conditionally fill column with custom function
#'
#' @param gtobj Object from `gt::gt()`
#' @param columns The names of the columns that are to be targeted.
#' @param FUN Function that returns a vector of color values.
#'   Note that the first two arguments should be `data` which will be `gtobj[["_data"]]`
#'   and `columns` which passes `columns`. Additional optinos can be passed from `...`
#'
#' @details If you have columns you want to reference in FUN that you don't want
#'   in the final table, use `gt::cols_hide()`
#'
#' @section Examples:
#'
#' Simple example that colors if a variable in the data equals some value
#'
#' ```{r, comment = "#>", collapse = TRUE, eval = FALSE}
#' color_if <- function(data, columns, var, value) {
#'   color <- rep("transparent", nrow(data))
#'   color[data[[var]] == value] <- "red"
#'   color
#' }
#'
#' gt(mtcars) %>%
#'   fill_column(column = "mpg", FUN = color_if, var = "cyl", val = 6)
#' ```
#'
#' @export
fill_column <- function(gtobj, columns, FUN, ...){
	data <- gtobj[["_data"]]

	color <- FUN(data = data, columns = columns, ...)

	# Loops through each row of `column` and color cells based on color vector
	for(i in seq_along(data)){
		gtobj <- gtobj %>%
			gt::tab_style(
				style = gt::cell_fill(color = color[i]),
				locations = gt::cells_body(columns = columns, rows = i)
			)
	}

	gtobj
}




