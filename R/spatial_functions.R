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
#' set.seed(1)
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

#' RcppParallel version of `st_distance`
#'
#' @param x. Object of class sf/sfc or a matrix of coordinates. If a
#'   matrix of coordinates, use column order from `st_coordinates()`.
#' @param y. Optional. Object of class sf/sfc or a matrix of coordinates. If a
#'   matrix of coordinates, use column order from `st_coordinates()`.
#'   If not included, x is used.
#' @param unit. Either "mi" or "km". Sets the output distance.
#'
#' @return Matrix of distances of dimension n_x by n_y.
#'
#' @section Examples:
#'
#' ```{r, comment = "#>"}
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package="sf"))
#' st_dist_rcpp(nc[1:3,])
#' ```
#'
#' ```{r, comment = "#>"}
#' nc_long <- nc[rep(seq_len(nrow(nc)), 5), ]
#' # massive speedup
#' microbenchmark::microbenchmark(
#'	 sf = sf::st_distance(nc_long),
#'   parallel = st_dist_rcpp(nc_long),
#'   times = 1
#' )
#' ```
#'
#' @export
st_dist_rcpp <- function(x, y = x, unit = "mi") {
	if(inherits(x, "sf")) {
		x <- st_geometry(x)
	}
	if(inherits(x, "sfc")) {
		if(!inherits(x, "sfc_POINT")) {
			x <- st_point_on_surface(x)
		}

		x <- st_coordinates(x)
		colnames(x) <- c("lon", "lat")
	}
	if(inherits(x, "data.frame")) {
		x <- as.matrix(x)
	}


	# Symmetric
	if(identical(x, y)) {
		dist <- rcpp_haversine_mat_symmetric(x[,"lon"], x[,"lat"], x[,"lon"], x[,"lat"])

	# Non-symmetric
	} else {
		if(inherits(y, "sf")) {
			y <- st_geometry(y)
		}
		if(inherits(y, "sfc")) {
			if(!inherits(y, "sfc_POINT")) {
				y <- st_point_on_surface(y)
			}

			y <- st_coordinates(y)
			colnames(y) <- c("lon", "lat")
		}
		if(inherits(y, "data.frame")) {
			y <- as.matrix(y)
		}

		dist <- rcpp_parallel_distm_C(x, y)
	}

	if(unit != "km") {
		cli::cli_alert("Distance in miles. To use kilometers use option `unit == 'km'`")
	} else {
		cli::cli_alert("Distance in kilometers. To use miles use option `unit == 'mi'`")
		dist <- dist / 0.621371
	}

	return(dist)
}

#' RcppParallel version of `st_nearest_feature`
#'
#' @param x. Object of class sf/sfc or a matrix of coordinates. If a
#'   matrix of coordinates, use column order from `st_coordinates()`.
#' @param y. Object of class sf/sfc or a matrix of coordinates. If a
#'   matrix of coordinates, use column order from `st_coordinates()`.
#'
#' @return Vector. Indices corresponding to row of y that is closest to each element of x.
#'
#' @section Examples:
#'
#' ```{r, comment = "#>"}
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package="sf"))
#' st_nearest_rcpp(nc, nc[4:6,])
#' ```
#'
#' @export
st_nearest_rcpp <- function(x, y) {

	if(inherits(x, "sf")) {
		x <- st_geometry(x)
	}
	if(inherits(x, "sfc")) {
		if(!inherits(x, "sfc_POINT")) {
			x <- st_point_on_surface(x)
		}

		x <- st_coordinates(x)
		colnames(x) <- c("lon", "lat")
	}
	if(inherits(x, "data.frame")) {
		x <- as.matrix(x)
	}


	# Symmetric
	if(inherits(y, "sf")) {
		y <- st_geometry(y)
	}
	if(inherits(y, "sfc")) {
		if(!inherits(y, "sfc_POINT")) {
			y <- st_point_on_surface(y)
		}

		y <- st_coordinates(y)
		colnames(y) <- c("lon", "lat")
	}
	if(inherits(y, "data.frame")) {
		y <- as.matrix(y)
	}

	idx <- rcpp_parallel_distm_C_min(x, y)

	return(idx)
}

#' RcppParallel distance to nearest point
#'
#' @param x. Object of class sf/sfc or a matrix of coordinates. If a
#'   matrix of coordinates, use column order from `st_coordinates()`.
#' @param y. Object of class sf/sfc or a matrix of coordinates. If a
#'   matrix of coordinates, use column order from `st_coordinates()`.
#' @param unit. Either "mi" or "km". Sets the output distance.
#'
#' @return Matrix of two columns. First column is index corresponding to the
#'   closest row of y. The second column is the distance to that element.
#'
#' @section Examples:
#'
#' ```{r, comment = "#>"}
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package="sf"))
#' st_nearest_distance_rcpp(nc, nc[4:6,])
#' ```
#'
#' @export
st_nearest_distance_rcpp <- function(x, y, unit = "mi") {

	if(inherits(x, "sf")) {
		x <- st_geometry(x)
	}
	if(inherits(x, "sfc")) {
		if(!inherits(x, "sfc_POINT")) {
			x <- st_point_on_surface(x)
		}

		x <- st_coordinates(x)
		colnames(x) <- c("lon", "lat")
	}
	if(inherits(x, "data.frame")) {
		x <- as.matrix(x)
	}


	# Symmetric
	if(inherits(y, "sf")) {
		y <- st_geometry(y)
	}
	if(inherits(y, "sfc")) {
		if(!inherits(y, "sfc_POINT")) {
			y <- st_point_on_surface(y)
		}

		y <- st_coordinates(y)
		colnames(y) <- c("lon", "lat")
	}
	if(inherits(y, "data.frame")) {
		y <- as.matrix(y)
	}

	mat <- rcpp_parallel_nearest_facility(x, y)

	if(unit != "km") {
		cli::cli_alert("Distance in miles. To use kilometers use option `unit == 'km'`")
		mat[,2] <- mat[,2] / 0.621371
	} else {
		cli::cli_alert("Distance in kilometers. To use miles use option `unit == 'mi'`")
	}

	return(mat)
}
