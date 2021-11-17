% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spatial_functions.R
\name{st_nearest_rcpp}
\alias{st_nearest_rcpp}
\title{RcppParallel version of \code{st_nearest_feature}}
\usage{
st_nearest_rcpp(x, y)
}
\arguments{
\item{x.}{Object of class sf/sfc or a matrix of coordinates. If a
matrix of coordinates, use column order from \code{st_coordinates()}.}

\item{y.}{Object of class sf/sfc or a matrix of coordinates. If a
matrix of coordinates, use column order from \code{st_coordinates()}.}
}
\value{
Vector. Indices corresponding to row of y that is closest to each element of x.
}
\description{
RcppParallel version of \code{st_nearest_feature}
}
\section{Examples}{
\if{html}{\out{<div class="r">}}\preformatted{library(sf)
nc <- st_read(system.file("shape/nc.shp", package="sf"))
}\if{html}{\out{</div>}}\preformatted{#> Reading layer `nc' from data source `/Library/Frameworks/R.framework/Versions/4.1/Resources/library/sf/shape/nc.shp' using driver `ESRI Shapefile'
#> Simple feature collection with 100 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
#> Geodetic CRS:  NAD27
}\if{html}{\out{<div class="r">}}\preformatted{st_nearest_rcpp(nc, nc[4:6,])
}\if{html}{\out{</div>}}\preformatted{#> Warning in st_point_on_surface.sfc(x): st_point_on_surface may not give correct results for longitude/latitude data
}\preformatted{#> Warning in st_point_on_surface.sfc(y): st_point_on_surface may not give correct results for longitude/latitude data
}\preformatted{#>   [1] 2 2 2 1 2 3 1 3 2 2 2 2 2 2 2 2 1 2 2 1 3 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 1 2 2 2 2 2 2 2 2 2 2 1 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#>  [72] 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 1 2 2 2 2 2 2 2 1 2 2 2 2 2
}
}
