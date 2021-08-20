#' Print Xaringan .Rmd/.html file using `pagedown::chrome_print`
#'
#' @param file The full file path to .Rmd/.html file
#' @param pdf_file Specify custom pdf output location. Otherwise the same
#'   location of file will be used
#'
#' @export
print_rmd <- function(file, pdf_file = NULL) {
	if(is.null(pdf_file)) {
		pdf_file = file
		pdf_file = str_replace(pdf_file, ".Rmd", ".pdf")
		pdf_file = str_replace(pdf_file, ".html", ".pdf")
	}

	# temp allows for easier error output
	temp = NULL
	try({
		temp = pagedown::chrome_print(
			file, pdf_file, wait = 10
		)
	})
	if(is.null(temp)) {
		cli::cli_alert_warning("{file} failed")
	}
}
