#' Creates sparse 0/1 matrix for factor variables
#'
#' @description
#' This function is a really fast and memory efficient way to create a
#'   0/1 matrix from a factor variable. This is helpful for clustered standard errors.
#'
#'   Original from: https://stackoverflow.com/a/23042363/8785011
#'
#' @param df dataframe containing factor columns that will be turned into sparse matrix.
#'    Note that the variables aren't required to be factor variables themselves.
#'    Make sure not to include other variables in df
#'
#' @return \code{\link[Matrix::sparseMatrix]{Matrix::sparseMatrix}} with 0/1 matrix
#'
#' @export
#' @examples
#' n <- 1e6
#' df <- data.frame(
#'  x = factor(sample(c("A", "B", "C"), n, TRUE)),
#' 	y = factor(sample(c("D", "E"), n, TRUE))
#' )
#' mat <- fact_to_sparse(df)
#'
fact_to_sparse <- function(df) {

	# Convert to factor variables
	df[,names(df)] <- lapply(df[,names(df)] , factor)

	# Number of levels for each variable
	nlevels <- sapply(df, nlevels)

	# Rows
	i <- rep(seq_len(n), ncol(df))

	# Columns
	j <-
		# converts factor to number for each variable
		unlist(lapply(df, as.integer)) +
		# and shifts that number by the number of levels from previous variables
		rep(cumsum(c(0, head(nlevels, -1))), each = n)

	# Value = 1
	x <- 1

	Matrix::sparseMatrix(i = i, j = j, x = x)

}
