#' Create sparse matrix from triplet of coordinates and value
#' @param i A vector of row membership
#' @param j A vector of column membership
#' @param x A vector of the entries that should go in (i, j)
#'
#' @examples
#' i <- sample(letters, 10, replace = TRUE)
#' j <- sample(1:100, 10, replace = TRUE)
#' create_sparse(i, j)
#'
#' @return A sparse matrix of class `dgCMatrix` from the Matrix package
create_sparse <- function(i, j, x = 1) {
  i_idx <- indexthis::to_index(i, items = TRUE)
  j_idx <- indexthis::to_index(j, items = TRUE)

  Matrix::sparseMatrix(
    i = i_idx$index, j = j_idx$index, x = x,
    dimnames = list(i_idx$items, j_idx$items)
  )
}
