#include <Rcpp.h>
#include <math.h>

using namespace Rcpp;

//' Convert degrees to radians
//'
//' @param degree Degree value
//' @return Radian value (double)
double deg_to_rad(const double& degree) {
	return (degree * M_PI / 180);
}

//' Compute Haversine distance between two points
//'
//' @param lat1 Latitude for starting point
//' @param lon1 Longitude for starting point
//' @param lat2 Latitude for ending point
//' @param lon2 Longitude for ending point
//' @return Double of distance between coordinate pairs in km
//' @export
// [[Rcpp::export]]
double haversine(double lat1, double lon1,
                        double lat2, double lon2)
{
	// distance between latitudes
	// and longitudes
	double dLat = (lat2 - lat1) *
		M_PI / 180.0;
	double dLon = (lon2 - lon1) *
		M_PI / 180.0;

	// convert to radians
	lat1 = (lat1) * M_PI / 180.0;
	lat2 = (lat2) * M_PI / 180.0;

	// apply formulae
	double a = pow(sin(dLat / 2), 2) +
		pow(sin(dLon / 2), 2) *
		cos(lat1) * cos(lat2);
	double rad = 6371;
	double c = 2 * asin(sqrt(a));
	return rad * c;
}

//'
//' @export
// [[Rcpp::export]]
NumericMatrix rcpp_haversine_mat(
		NumericVector lon1, NumericVector lat1,
		NumericVector lon2, NumericVector lat2) {

	NumericMatrix rmat(lon1.length(), lon2.length());

	for (int i = 0; i < lon1.length(); i++) {
		for (int j = 0; j < lon2.length(); j++) {
			double d = haversine(lat1[i], lon1[i], lat2[j], lon2[j]);
			rmat(i,j) = d;
		}
	}
	return(rmat);
}

//'
//' @export
// [[Rcpp::export]]
NumericMatrix rcpp_haversine_mat_symmetric(
		NumericVector lon1, NumericVector lat1,
		NumericVector lon2, NumericVector lat2) {

	NumericMatrix rmat(lon1.length(), lon2.length());

	for (int i = 0; i < lon1.length(); i++) {
		for (int j = 0; j < i; j++) {
			double d = haversine(lat1[i], lon1[i], lat2[j], lon2[j]);
			rmat(i,j) = d;
			rmat(j,i) = d;
		}
	}
	return(rmat);
}

