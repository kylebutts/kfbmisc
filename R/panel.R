#' From an indicator for treatment, retrieve start year.
#'
#' @description This function returns the earliest year where an indicator == 1.
#'   This is most useful with `group_by(unit) |> mutate(g = ...)` or
#'   `DT(j = g := ..., by = unit)` to convert between D and g variable in panel data.
#'
#' @param y Vector of years
#' @param t Vector of 0/1 for treatment
#'
#' @return Returns earliest y for which t == 1
#'
#' @export
get_min_year = function(y, t) {
	return(y[order(y)][min(which(t[order(y)] == 1))])
}
