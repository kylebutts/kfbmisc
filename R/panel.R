#' From an indicator for treatment, retrieve start year.
#'
#' @description This function returns the earliest year where an indicator == 1.
#'   If indicator never equals zero, return Inf.
#'   This is most useful with 
#'   `mutate(.by = unit, g = get_min_year(year, treat))` or
#'   `DT(j = g := get_min_year(year, treat), by = unit)` 
#'   to convert between d and g variable in panel data.
#'
#' @param t Vector of years
#' @param d Vector of 0/1 for treatment
#'
#' @return Returns earliest y for which t == 1
#'
#' @export
get_min_year <- function(t, d) {
  min <- t[order(t)][min(which(d[order(t)] == 1))]
  if (is.na(min)) min <- Inf
  return(min)
}
