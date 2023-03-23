#' @title Prints vector with each item on a new line
#'
#' @param vec Vector you want to print
#'
#' @export
copy_vec <- function(vec) {
  if (interactive()) {
    vec |>
      paste0(collapse = "\n") |>
      clipr::write_clip()
    cli::cli_alert_success("Copied to clipboard")
  }
}
