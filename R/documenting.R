#' Section headers
#'
#' @details `h1` has the following output that is automatically copied to the clipboard
#'
#' `# ------------------------------------------------------------------------------`
#'
#' `# txt`
#'
#' `# ------------------------------------------------------------------------------`
#'
#' @param txt Text you want to wrap in my header format
#'
#' @export
h1 <- function(txt) {
	str <- paste0(
		"# ", paste(rep("-", 78), collapse = ""), "\n",
		"# ", txt, "\n",
		"# ", paste(rep("-", 78), collapse = ""), "\n",
		collapse = ""
	)

	cli::cli({
		cli::cli_alert("Copied the following to the clipboard:")
		cli::cli_code(str)
	})

	clipr::write_clip(str)
}

#' Subsection headers
#'
#' @details `h2` has the following output that is automatically copied to the clipboard
#'
#' `# ---- txt ---------------------------------------------------------------------`
#'
#' @param txt Text you want to wrap in my header format
#'
#' @export
h2 <- function(txt) {
	str <- paste0(
		"# ---- ",
		txt, " ",
		paste(rep("-", 80 - 8 - nchar(txt)), collapse = ""),
		collapse = ""
	)

	cli::cli({
		cli::cli_alert("Copied the following to the clipboard:")
		cli::cli_code(str)
	})

	clipr::write_clip(str)
}

