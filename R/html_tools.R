#' Convert raw html to htmltools/shiny tags
#'
#' @description Note that this copies text to your clipboard
#'
#' @param raw_html Character vector of html text
#'
#' @return Character string of htmltools:: version of html
#'
#' @examples
#' raw1 <- div(
#'   class = "max-w-6xl",
#'   div(
#'     class = "prose", style = "width:100%;",
#'     h1("Header"),
#'     "prose"
#'   ),
#'   div(
#'     class = "prose",
#'     h2("Smaller Header"),
#'     p("Testing ", span(class = "italic", "this function")),
#'     img(src = "cat.png", 'custom-attr' = 1)
#'   )
#' ) |>
#'   as.character()
#'
#' html2R(raw1)
#'
#' raw2 <- r"(<div class="container-fluid max-w-6xl"><div class="prose" style="width:100%;"><h1>Header</h1>prose</div><div class="prose"><h2>Smaller Header</h2><p>Testing<span class="italic">this function</span></p><img src="cat.png" custom-attr="1"/></div></div>)"
#'
#' html2R(raw2)
#'
#' @export
html2R <- function(raw_html) {

	html <-
		xml2::read_html(raw_html) |>
		rvest::html_nodes("head,body,footer") |>
		as.character() |>
		stringr::str_replace_all(stringr::regex("<!--(.*?)-->"), "") |>
		stringr::str_replace_all("\\n", "") |>
		stringr::str_split("(?=<)") |>
		unlist() |>
		stringr::str_trim() |>
		stringr::str_squish()

	line <- html |>
		# Remove empty lines and <body> tag that read_html creates
		(\(line) { line[line != "" & !stringr::str_detect(line, "<body|</body")] })() |>
		# Fix tags with />
		# @TODO: Check if </img> is there?
		stringr::str_replace("<img (.*?)>", "<img \\1\\/>") |>
		stringr::str_replace("<hr(.*?)>", "<hr \\1\\/>") |>
		# Embrace strings with "".
		stringr::str_replace_all(">(.+)", '>, \\"\\1\\",') |>
		# Replace </tag> with ), and /> with ),
		stringr::str_replace_all("</[a-zA-Z0-9]+>", "),") |>
		# Replace <img /> and <span /> with tags$
		stringr::str_replace_all("<(.*?) (.*?)/>", "tags$\\1\\(\\2\\),") |>
		# Replace opening < with tags$
		stringr::str_replace("<([a-zA-Z0-9]+)>", "tags$\\1(") |>
		# Add commas between attributes
		stringr::str_replace_all('([a-zA-Z-]*?)="(.*?)"', '\\1=\\"\\2\\",') |>
		stringr::str_replace("<(.*?) (.*?)?>", "tags$\\1\\( \\2") |>
		# enquote attribute names with -
		stringr::str_replace_all("([a-zA-z0-9]*?)-([a-zA-z0-9]*?)=\"(.*?)\",", "'\\1-\\2'=\"\\3\",") |>
		# remove double commas
		stringr::str_replace_all(",,", ",") |>
		stringr::str_replace_all("tags\\$(.*?)\\(,", "tags$\\1(") |>
		paste0(collapse = "") |>
		# Remove ,)
		stringr::str_replace_all(",\\)", ")") |>
		# Remove last ,
		stringr::str_remove(",$") |>
		# new lines by tags$
		stringr::str_split("(?=tags\\$)") |>
		(\(x) paste0(x[[1]], collapse="\n"))() |>
		# remove tags$ when not needed
		stringr::str_remove_all("tags\\$(?=div|p|h1|h2|h3|h4|h5|h6|a|img|br|span|pre|code|strong|em|hr\\()") |>
		# remove new line at start
		stringr::str_remove("^\n")


	# Wrap with tagList
	line <- paste0("htmltools::tagList( ", line, " )")

	# Indent properlly using styler
	line <- styler::style_text(line)

	cli::cli_alert("Copied htmltools taglist to clipboard")
	clipr::write_clip(line)

	line
}





#' Preview html string in RStudio viewer pane
#'
#' @description
#'
#' This function lets you preview html in the Viewer tab of RStudio. This
#' is helpful when creating things with custom css using htmltools
#' (or modifying gt tables)
#'
#' @param html A valid html string. Can be a `character`, `html` object from
#'   `gt::html`, or `shiny.tag` from `htmltools`.
#'
#' @section Examples:
#'
#' ```{r results = "hold", comment = "#>"}
#' library(fontawesome)
#' library(htmltools)
#' more_or_less_css <- css(
#'     font.size = "13px",
#'     width = "80px",
#'     font.family = "arial",
#'     display = "flex",
#'     flex.direction = "column",
#'     justify.content = "center",
#'     text.align = "center",
#'     padding.right = "5px",
#'     color = "#999"
#' )
#'
#' more_or_less <- div(
#'     style = more_or_less_css,
#'     div(
#'         style = css(padding.bottom = "20px"),
#'         fontawesome::fa("angle-double-up"),
#'         p("Increasing")
#'     ),
#'     div(
#'         style = css(padding.top = "40px"),
#'         p("Decreasing"),
#'         fontawesome::fa("angle-double-down")
#'     )
#' )
#'
#' # preview_html(more_or_less)
#' ```
#'
#' @export
preview_html <- function(html) {

  # Convert to character
  if (inherits(html, "html") | inherits(html, "shiny.tag")) {
    html <- as.character(html)
  }

  # Confirm html is character
  if (!is.character(html)) {
    stop("html has to be a character")
  }

  if (interactive()) {
    htmlFile <- tempfile("index", fileext = ".html")
    writeLines(html, con = htmlFile)

    rstudioapi::viewer(htmlFile)
  } else {
    stop("This function can only be run in an interactive session")
  }
}
