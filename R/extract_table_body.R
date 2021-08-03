#' Extract table body from \code{\link[fixest:etable]{fixest::etable}}
#'
#' @param tab The result of \code{\link[fixest:etable]{fixest::etable}} with
#'   `tex = TRUE`
#'
#' @section Examples:
#'
#' ```{r, comment = "#>"}
#' est <- fixest::feols(hp ~ 1 + mpg, data = mtcars)
#' tab <- fixest::etable(est, tex = TRUE, fitstat = c("n", "ar2"))
#'
#' # export to file with file option of cat
#' cat(extract_body_etable(tab))
#'
#' est_fe <- fixest::feols(hp ~ 1 + mpg | cyl, data = mtcars)
#' tab_fe <- fixest::etable(est_fe, tex = TRUE, fitstat = c("n", "ar2"))
#' cat(extract_body_etable(tab_fe))
#' ```
#'
#' @export
extract_body_etable <- function(tab) {
	tab <- paste0(tab, collapse = "\n")

	stringr::str_match(
		tab,
		"\\\\midrule\\\\midrule\\n\\n([.\\s\\S\\n]*?)\\n\\n\\\\midrule\\\\midrule"
	)[[2]]
}




#' Extract table body from \code{\link[gt::gt]{gt::gt}}
#'
#' @param tab The result of \code{\link[gt::gt]{gt::gt}} or after manually
#'   calling `as.character(gt::as_latex(tab))`
#'
#' @section Examples:
#'
#'
#' ## gt
#'
#' ```{r, comment = "#>"}
#' tab <- gt::gt(head(mtcars))
#'
#' # export to file with file option of cat
#' cat(extract_body_gt(tab))
#' ```
#'
#' ## gtsummary
#'
#' ```{r, comment = "#>"}
#' library(gtsummary)
#' tab <- tbl_summary(
#'   mtcars[,c("cyl", "hp", "mpg")],
#'   statistic = list(all_continuous() ~ "{mean} ({sd})"),
#'   by = "cyl"
#' ) %>%
#'   # remove footnotes
#'   modify_footnote(update = everything() ~ NA)
#'
#' cat(extract_body_gt(tab))
#' ```
#'
#' @export
extract_body_gt <- function(tab) {

	# from gtsummary::tbl_summary(), convert to gt_tbl
	if(inherits(tab, "tbl_summary")) tab <- gtsummary::as_gt(tab)

	# from gt::gt()
	if(inherits(tab, "gt_tbl")) tab <- as.character(gt::as_latex(tab))


	stringr::str_match(
		tab,
		"\\\\toprule\\n([.\\s\\S\\n]*?)\\n\\\\bottomrule"
	)[[2]]
}
