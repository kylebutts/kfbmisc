// [[Rcpp::depends(RcppParallel)]]
#define STRICT_R_HEADERS
#include <RcppParallel.h>
#include <Rcpp.h>
using namespace RcppParallel;
using namespace Rcpp;

// From https://github.com/mkuehn10/pargeodist/blob/master/pargeodist.cpp

//' Haversine Distance
//'
//' @param lon_x Longitude for starting point
//' @param lat_x Latitude for starting point
//' @param lon_y Longitude for ending point
//' @param lat_y Latitude for ending point
//'
//' @return Double of distance between points in m
//'
// [[Rcpp::export]]
double dist_haversine(double lon_x, double lat_x, double lon_y, double lat_y) {
	double R = 6378137.0;
	double phi_1, phi_2, delta_phi, delta_lambda, a, c;

	// convert to radians
	lon_x = lon_x * (M_PI / 180);
	lat_x = lat_x * (M_PI / 180);
	lon_y = lon_y * (M_PI / 180);
	lat_y = lat_y * (M_PI / 180);

	// great-circle distance
	// https://en.wikipedia.org/wiki/Haversine_formula
	// Use formula from here:
	// https://www.movable-type.co.uk/scripts/latlong.html
	phi_1 = lat_x;
	phi_2 = lat_y;
	delta_phi = (lat_y - lat_x);
	delta_lambda = (lon_y - lon_x);
	a = sin(delta_phi / 2.0) * sin(delta_phi / 2.0) +
		cos(phi_1) * cos(phi_2) * sin(delta_lambda / 2.0) *
		sin(delta_lambda / 2.0);
	c = 2.0 * atan2(sqrt(a), sqrt(1 - a));
	return R * c;
}

//' Spherical Distance
//'
//' @param lon_x Longitude for starting point
//' @param lat_x Latitude for starting point
//' @param lon_y Longitude for ending point
//' @param lat_y Latitude for ending point
//'
//' @return Double of distance between points in m
//'
// [[Rcpp::export]]
double dist_spherical_cosine(double lon_x, double lat_x, double lon_y, double lat_y) {
	//double R = 6378137;
	double R = 6371000;

	// convert to radians
	lon_x = lon_x * (M_PI / 180);
	lat_x = lat_x * (M_PI / 180);
	lon_y = lon_y * (M_PI / 180);
	lat_y = lat_y * (M_PI / 180);

	return acos(sin(lat_x) * sin(lat_y) + cos(lat_x) * cos(lat_y) * cos(lon_y - lon_x)) * R;
}

struct myDistanceMatrix : public Worker {
	RMatrix<double> x;
	RMatrix<double> y;
	RMatrix<double> rmat;

	myDistanceMatrix(NumericMatrix x, NumericMatrix y, NumericMatrix rmat)
		: x(x), y(y), rmat(rmat) {}

	void operator()(std::size_t begin, std::size_t end) {
		for (std::size_t i = begin; i < end; i++) {
			for (std::size_t j = 0; j < y.nrow(); j++) {
				double dist = dist_haversine(x(i, 0), x(i, 1), y(j, 0), y(j, 1));
				rmat(i, j) = dist / 1609.34;
			}
		}
	}
};

//' Parallel calculates distance between two matrices of lat/long.
//'
//' @param x matrix of coordinates. In the same format as st_coordinates output (X, Y)
//' @param y matrix of coordinates. In the same format as st_coordinates output (X, Y)
//'
//' @return returns matrix of distances in miles
//'
// [[Rcpp::export]]
NumericMatrix rcpp_parallel_distm_C(NumericMatrix x, NumericMatrix y) {
	NumericMatrix rmat(x.nrow(), y.nrow());

	myDistanceMatrix my_distance_matrix(x, y, rmat);

	parallelFor(0, rmat.nrow(), my_distance_matrix, 1);

	return rmat;
}


struct myDistanceVector : public Worker {
	RMatrix<double> x;
	RMatrix<double> y;
	RVector<int> rvec;

	myDistanceVector(NumericMatrix x, NumericMatrix y, IntegerVector rvec)
		: x(x), y(y), rvec(rvec) {}

	void operator()(std::size_t begin, std::size_t end) {
		double dist = 0;
		double min = 0;
		int pos = 0;

		for (std::size_t i = begin; i < end; i++) {
			for (std::size_t j = 0; j < y.nrow(); j++) {
				dist = dist_haversine(x(i, 0), x(i, 1), y(j, 0), y(j, 1));

				// if on first position in row, set minimum to the first value
				// set pos to 0 + 1 (adjust by 1 for returning back to R)
				if (j == 0) {
					min = dist;
					pos = j + 1;

					// else if encounter a smaller distance, set min and pos to
					// current distance and position (+1 again to account for array
					// indexing)
				} else if (dist < min) {
					min = dist;
					pos = j + 1;
				}
			}

			rvec[i] = pos;
		}
	}
};

//'  Find closest unit from group y
//'
//'  @param x matrix of coordinates. In the same format as st_coordinates output (X, Y)
//'  @param y matrix of coordinates. In the same format as st_coordinates output (X, Y)
//'
//'  @return Vector. Contains index of closest unit in y
//'
//'  @export
// [[Rcpp::export]]
IntegerVector rcpp_parallel_distm_C_min(NumericMatrix x, NumericMatrix y) {
	IntegerVector rvec(x.nrow());

	myDistanceVector my_distance_vector(x, y, rvec);

	parallelFor(0, x.nrow(), my_distance_vector, 1);

	return rvec;
}


struct myDistanceVectorNonself : public Worker {
	RMatrix<double> x;
	RMatrix<double> y;
	RVector<int> rvec;

	myDistanceVectorNonself(NumericMatrix x, NumericMatrix y, IntegerVector rvec)
		: x(x), y(y), rvec(rvec) {}

	void operator()(std::size_t begin, std::size_t end) {
		double dist = 0;
		double min = 0;
		int pos = 0;

		for (std::size_t i = begin; i < end; i++) {
			for (std::size_t j = 0; j < y.nrow(); j++) {

				dist = dist_haversine(x(i, 0), x(i, 1), y(j, 0), y(j, 1));

				// if on first position in row, set minimum to the first value
				// set pos to 0 + 1 (adjust by 1 for returning back to R)
				// Nonself => don't set j = 0 for i = 0
				if ( (j == 0 & i > 0) | (i == 0 & j == 1)) {
					min = dist;
					pos = j + 1;

				// else if encounter a smaller distance, set min and pos to
				// current distance and position (+1 again to account for array
				// indexing)
				// Nonself => dist > 0
				} else if (dist < min & dist > 0) {
					min = dist;
					pos = j + 1;
				}

			}

			rvec[i] = pos;
		}
	}
};

//'  Find nearest neighbor of x (non-self)
//'
//'  @param x matrix of coordinates. In the same format as st_coordinates output (X, Y)
//'  @param y Same as x.
//'
//'  @return Vector. Contains index of closest unit in y
//'
//'  @export
// [[Rcpp::export]]
IntegerVector rcpp_parallel_distm_C_min_nonself(NumericMatrix x, NumericMatrix y) {
	IntegerVector rvec(x.nrow());

	myDistanceVectorNonself my_distance_vector(x, y, rvec);

	parallelFor(0, x.nrow(), my_distance_vector, 1);

	return rvec;
}


struct facilityMatrix : public Worker {
	RMatrix<double> x;
	RMatrix<double> y;
	RMatrix<double> rmat;

	//int miles;

	facilityMatrix(NumericMatrix x, NumericMatrix y, NumericMatrix rmat)
		: x(x), y(y), rmat(rmat) {}

	void operator()(std::size_t begin, std::size_t end) {
		double dist = 0;
		double min = 0;
		int pos = 0;

		for (std::size_t i = begin; i < end; i++) {
			for (std::size_t j = 0; j < y.nrow(); j++) {

				//dist = dist_haversine(x(i, 0), x(i, 1), y(j, 0), y(j, 1));
				dist = dist_haversine(x(i, 0), x(i, 1), y(j, 0), y(j, 1));

				// if on first position in row, set minimum to the first value
				// set pos to 0 + 1 (adjust by 1 for returning back to R)
				if (j == 0) {
					min = dist;
					pos = j + 1;

					// else if encounter a smaller distance, set min and pos to
					// current distance and position (+1 again to account for array
					// indexing)
				} else if (dist < min) {
					min = dist;
					pos = j + 1;
				}
			}

			rmat(i,0) = pos;
			rmat(i,1) = min / 1609.34;
		}
	}
};

//'  Find closest unit from group y and measure distance
//'
//'  @param x Matrix of coordinates. In the same format as st_coordinates output (X, Y)
//'  @param y Matrix of coordinates. In the same format as st_coordinates output (X, Y)
//'
//'  @return Matrix. 2 columns. First column is index of closest unit in y.
//'  Second column is distance
//'
//'  @export
// [[Rcpp::export]]
NumericMatrix rcpp_parallel_nearest_facility(NumericMatrix x, NumericMatrix y) {
	NumericMatrix rmat(x.nrow(), 2);

	facilityMatrix my_distance_matrix(x, y, rmat);

	parallelFor(0, rmat.nrow(), my_distance_matrix, 1);

	return rmat;
}


struct facilityMatrixNonself : public Worker {
	RMatrix<double> x;
	RMatrix<double> y;
	RMatrix<double> rmat;

	//int miles;

	facilityMatrixNonself(NumericMatrix x, NumericMatrix y, NumericMatrix rmat)
		: x(x), y(y), rmat(rmat) {}

	void operator()(std::size_t begin, std::size_t end) {
		double dist = 0;
		double min = 0;
		int pos = 0;

		for (std::size_t i = begin; i < end; i++) {
			for (std::size_t j = 0; j < y.nrow(); j++) {

				//dist = dist_haversine(x(i, 0), x(i, 1), y(j, 0), y(j, 1));
				dist = dist_haversine(x(i, 0), x(i, 1), y(j, 0), y(j, 1));

				// if on first position in row, set minimum to the first value
				// set pos to 0 + 1 (adjust by 1 for returning back to R)
				// Nonself => don't set j = 0 for i = 0
				if ( (j == 0 & i > 0) | (i == 0 & j == 1)) {
					min = dist;
					pos = j + 1;

				// else if encounter a smaller distance, set min and pos to
				// current distance and position (+1 again to account for array
				// indexing)
				// Nonself => dist > 0
				} else if (dist < min & dist > 0) {
					min = dist;
					pos = j + 1;
				}
			}

			rmat(i,0) = pos;
			rmat(i,1) = min / 1609.34;
		}
	}
};

//'  Find nearest neighbor of x and measure distance (non-self)
//'
//'  @param x matrix of coordinates. In the same format as st_coordinates output (X, Y)
//'  @param y Same as x.
//'
//'  @return Matrix. 2 columns. First column is index of closest unit in y.
//'  Second column is distance
//'
//'  @export
// [[Rcpp::export]]
NumericMatrix rcpp_parallel_nearest_facility_nonself(NumericMatrix x, NumericMatrix y) {
	NumericMatrix rmat(x.nrow(), 2);

	facilityMatrixNonself my_distance_matrix(x, y, rmat);

	parallelFor(0, rmat.nrow(), my_distance_matrix, 1);

	return rmat;
}
