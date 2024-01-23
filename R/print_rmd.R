#' Print Xaringan .Rmd/.html file using `pagedown::chrome_print`
#'
#' @param file The full file path to .Rmd/.html file
#' @param pdf_file Specify custom pdf output location. Otherwise the same
#'   location of file will be used
#'
#' @export
print_rmd <- function(file, pdf_file = NULL) {
  if (is.null(pdf_file)) {
    pdf_file <- paste0(xfun::with_ext(file, ".pdf"), ".pdf")
  }
  
  # temp allows for easier error output
  print("Attempting to print to pdf")
  temp <- NULL
  try({
    temp <- pagedown::chrome_print(
      file, pdf_file,
      wait = 10
    )
  })

  if (is.null(temp)) {
    warning(paste0(file, "failed"))
    return(invisible(FALSE))
  }
  
  return(invisible(TRUE))
}
