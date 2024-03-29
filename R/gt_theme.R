

#' Custom gt theme which borrows heavily from Thomas Mock
#'
#' @param data `gt` object. Pipe into this function to add theme
#' @param ... Additional arguments to pass to `gt::tab_options`
#'
#' @examples
#'
#' library(gt)
#'
#' if (interactive()) {
#'   mtcars[1:5, ] |>
#'     gt() |>
#'     gt_theme_kyle() |>
#'     tab_header(title = make_gt_title("mtcars Dataset")) |>
#'     tab_source_note(source_note = "Data from mtcars in R")
#' }
#'
#' @export
gt_theme_kyle <- function(data, ...) {
  data |>
    gt::opt_all_caps() |>
    gt::opt_table_font(
      font = list(
        gt::google_font("Fira Code"),
        gt::default_fonts()
      )
    ) |>
    gt::tab_options(
      table.border.top.width = gt::px(3),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = gt::px(3),
      column_labels.border.top.width = gt::px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = gt::px(3),
      column_labels.border.bottom.color = "black",
      data_row.padding = gt::px(5),
      source_notes.font.size = 12,
      table.font.size = 16,
      heading.align = "left",
      ...
    )
}


#' Helper function to make gt title with gt_theme_kyle
#'
#' @param title Character containing title.
#'
#' @export
make_gt_title <- function(title) {
  html <- htmltools::span(style = "display: block; margin-bottom: 8px;", title)
  gt::html(as.character(html))
}
