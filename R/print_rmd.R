#' Print Xaringan .Rmd/.html file using `pagedown::chrome_print`
#'
#' @param file The full file path to .Rmd/.html file
#' @param pdf_file Specify custom pdf output location. Otherwise the same
#'   location of file will be used
#'
#' @export
<<<<<<< HEAD
print_rmd <- function(file, pdf_file = NULL, verbose = TRUE) {
=======
print_rmd <- function(file, pdf_file = NULL, verbose = FALSE) {
>>>>>>> 684aec1f3d91b881afa2c9ef9b7de61e04666cde
  if (is.null(pdf_file)) {
    pdf_file <- fs::path_ext_set(file, ".pdf")
  }
  html_file <- fs::path_ext_set(file, ".html")
  file <- "/Users/kylebutts/Documents/Mixtape-Sessions/Mixtape-Documents/Invoice.Rmd"

  search = fs::dir_ls(
    fs::path_dir(file), 
    regexp = paste0(fs::path_ext_remove(file), "\\.[Rq]md")
  )

  Rmd_file = NULL
  if (length(search) > 0) {
    Rmd_file = search[1]
  }

  file_to_print <- file
  if (
    fs::file_exists(html_file) && 
    fs::file_info(html_file)$modification_time > fs::file_info(Rmd_file)$modification_time
  ) {
    file_to_print = html_file
  }

  # temp allows for easier error output
<<<<<<< HEAD
  if (verbose) print("Attempting to print to pdf")
=======
  verbose && print("Attempting to print to pdf")

>>>>>>> 684aec1f3d91b881afa2c9ef9b7de61e04666cde
  
  temp <- NULL
  try({
    temp <- pagedown::chrome_print(
      file_to_print, pdf_file,
      wait = 10
    )
  })

  if (is.null(temp)) {
    warning(paste0(file, "failed"))
    return(invisible(FALSE))
  }
  
  return(invisible(TRUE))
}
