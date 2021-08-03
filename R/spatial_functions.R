#' Extract information from geometry that a point x falls in
#'
#' @description
#'   For each point in x, identify which geometry in y the point falls within
#'   and extract value of variable `var`. This is good, for example, if you have
#'   points and you want to extract information about the county/census tract/etc.
#'
#' @param x `sf` object consisting of points
#' @param y `sf` object consisting of geometries
#' @param var character of variable name you wish to extract from `y`
#'
#' @section Examples:
#'
#' ```{r, comment = "#>"}
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package="sf"))
#' x <- st_as_sf(data.frame(id = 1:10, geometry = st_sample(nc, 10)))
#' x$CNTY_ID = st_extract_within(x, nc, "CNTY_ID")
#' x
#' ```
#'
#' @export
st_extract_within <- function(x, y, var) {
	if(!inherits(x, "sf") | !inherits(y, "sf")) {
		stop("Both x and y must be sf objects")
	}

	temp = sf::st_within(x, y)
	# Replace NULL with NA for unlist
	temp[sapply(temp, function(x) length(x)==0L)] <- NA

	val = rep(NA_real_, length(temp))
	idx = unlist(temp)



	val[!is.na(idx)] = sf::st_drop_geometry(y)[idx,][[var]]

	return(val)
}

#' Returns an indicator vector for whether y contains a point of x using st_within
#'
#' @param x `sf` object consisting of points
#' @param y `sf` object consisting of geometries
#'
#' @section Examples:
#'
#' ```{r, comment = "#>"}
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package="sf"))
#' x <- st_as_sf(data.frame(id = 1:10, geometry = st_sample(nc, 10)))
#' nc$contains_x = st_indicator_for_within(x, nc)
#' nc
#' ```
#'
#' @export
st_indicator_for_within <- function(x, y) {
	if(!inherits(x, "sf") | !inherits(y, "sf")) {
		stop("Both x and y must be sf objects")
	}

	temp = st_within(x, y)

	idx = unlist(temp)

	ind = rep(0, nrow(y))

	ind[idx] = 1

	return(ind)
}

#' Multiplies a sparse geometry binary predicate object (e.g. from `sf::st_within`) and a vector
#'
#' @description
#'   Performs sgbp %*% mat, where sgbp is a 0/1 matrix.
#'   The main use case is when you want to sum a variable from points into a
#'   geometry. For example, sgbp %*% rep(1, n) will give you the number within
#'   each geometry.
#'
#' @param sgbp `sgbp` object from an `sf` function, e.g. `sf::st_within`
#' @param mat Vector/Matrix you want to perform sgbp %*% mat.
#'
#' @export
st_multiply_sgbp <- function(sgbp, mat) {
	mat = as.matrix(mat)

	# mat[x, ] selects only the rows with sgbp = TRUE
	ret = lapply(sgbp, function(x) mat[x, ])

	# colSums is multiplying the non-sparse 1,0 matrix
	if(dim(mat)[2] == 1){
		ret = lapply(ret, function(x) sum(x))
	}else {
		ret = lapply(ret, function(x) colSums(x))
	}

	ret = do.call(rbind, ret)

	return(ret)
}
