#' Intro block for top of script
#'
#' @details `introblock` has the following output that is automatically copied to the clipboard
#'
#' `## filename.R ------------------------------------------------------------------`
#' `## Kyle Butts, CU Boulder Economics`
#' `## `
#' `## txt`
#'
#' @param filename Name of file (including .R)
#' @param author Name of author. Defaults to me
#' @param txt Optional, text description of file
#'
#' @export
introblock <- function(filename, author = "Kyle Butts, CU Boulder Economics", txt = "") {
  file <- paste0(
    "## ",
    filename, " ",
    paste(rep("-", 80 - 4 - nchar(filename)), collapse = ""),
    collapse = ""
  )
  author <- paste0(
    "## ", author,
    collapse = ""
  )
  line <- "## "
  txt <- paste0(
    "## ", txt,
    collapse = ""
  )

  str <- paste0(file, "\n", author, "\n", line, "\n", txt, collapse = "")

  if (interactive()) {
    cli::cli({
      cli::cli_alert("Copied the following to the clipboard:")
      cli::cli_code(str)
    })

    clipr::write_clip(str)
  }
}

#' Section headers
#'
#' @details `h1` has the following output that is automatically copied to the clipboard
#'
#' `# ------------------------------------------------------------------------------`
#' `# txt`
#' `# ------------------------------------------------------------------------------`
#'
#' @param txt Text you want to wrap in my header format
#'
#' @export
header1 <- function(txt) {
  str <- paste0(
    "# ", paste(rep("-", 78), collapse = ""), "\n",
    "# ", txt, "\n",
    "# ", paste(rep("-", 78), collapse = ""), "\n",
    collapse = ""
  )

  if (interactive()) {
    cli::cli({
      cli::cli_alert("Copied the following to the clipboard:")
      cli::cli_code(str)
    })

    clipr::write_clip(str)
  }
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
header2 <- function(txt) {
  str <- paste0(
    "# ---- ",
    txt, " ",
    paste(rep("-", 80 - 8 - nchar(txt)), collapse = ""),
    collapse = ""
  )

  if (interactive()) {
    cli::cli({
      cli::cli_alert("Copied the following to the clipboard:")
      cli::cli_code(str)
    })

    clipr::write_clip(str)
  }
}

# Not for export (to avoid name conflicts)
h1 <- header1
h2 <- header2
