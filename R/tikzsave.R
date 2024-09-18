# TODO: Figure out tikzDictionary when using Rmarkdown
# TODO: pdf_to_png using `convert -density 300 mydocument.pdf mydocument.png`

delete_if_exists <- function(filename) {
  if (file.exists(filename)) file.remove(filename)
}

tikzsave_minimal_required_preamble <- c(
  "\\usepackage{tikz}",
  "\\usepackage{pgfplots}",
  "\\pgfplotsset{compat=1.18}"
)

#' Save a tikzpicture and then compile to pdf
#'
#' Uses `ggplot2::ggsave` with `tikzDevice::tikz` to create the tikz figure
#' and then compiles using `latexmk`.
#'
#' @inheritParams ggplot2::ggsave
#' @param packages Vector of strings. If no `\` is detected, then this is
#' assumed to be an absolute file paths to a `.sty` file to
#' include in preamble. Default is my `paper.sty` and `math.sty` in
#' `inst/tikzsave`.
#' @param pre_begin_document Verbatim text to include right before
#' `\begin{document}`
#' @param recompile Logical. Since `tikzDevice::tikz` caches effectively,
#' sometimes the `.pdf` file will be newer than the saved `.tex` file.
#' If `FALSE`, this will skip recomipilation in these cases.
#' Default is `TRUE`.
#' @param create_png Logical. If true, will also output a png in addition to
#' the pdf.
#' @param quiet Logical. If `TRUE` (default), do not display info messages.
#' @param ... Passed to `ggplot2::ggsave`
#'
#' @return Invisibly returns filename
#' @export
#'
tikzsave <- function(filename, plot = ggplot2::last_plot(), packages = NULL, pre_begin_document = NULL, recompile = TRUE, create_png = FALSE, quiet = TRUE, ...) {
  base <- basename(filename)
  dir <- here::here(dirname(filename))
  fs::dir_create(dir)

  # Replace `.pdf` with `.tex`
  filename <- fs::path_ext_set(here::here(dir, base), ".tex")

  # plot -> tikzpicture
  if (!quiet) message("Saving tikzpicture")
  ggplot2::ggsave(
    filename = filename, plot = plot,
    device = function(...) {
      tikzDevice::tikz(..., standAlone = FALSE, verbose = FALSE)
    },
    ...
  )

  if (!quiet) message("Compiling tikzpicture to pdf")
  compile_tikzpicture(
    filename = filename,
    packages = packages,
    recompile = recompile,
    create_png = create_png
  )

  return(invisible(filename))
}

#' Wraps and compiles a tikzpicture
#'
#' Takes the output of `tikzDevice::tikz` with `standalone = FALSE`, wraps into
#' a latex document and then compiles using `latexmk`, cleaning up as it goes.
#'
#' @param filename Character. Filename of a tikzpicture in `.tex` file
#' @param packages Vector of strings. If no `\` is detected, then this is
#' assumed to be an absolute file paths to a `.sty` file to
#' include in preamble. Default is my `paper.sty` and `math.sty` in
#' `inst/tikzsave`.
#' @param pre_begin_document Verbatim text to include right before
#' `\begin{document}`
#' @param recompile Logical. If FALSE, does not update if pdf date
#' is newer than the tex date.
#' @param create_png Logical. If true, will also output a png in addition to
#' the pdf.
#'
#' @return Invisibly returns `TRUE` if compilation succeeds.
#'
#' @export
compile_tikzpicture <- function(filename, packages = NULL, pre_begin_document = NULL, recompile = TRUE, create_png = FALSE) {
  tex_base <- fs::path_ext_remove(fs::path_file(filename))
  output_dir <- here::here(fs::path_dir(filename))
  fs::dir_create(output_dir)

  if (is.null(packages)) {
    packages <- c(
      system.file("tikzsave/paper.sty", package = "kfbmisc"),
      system.file("tikzsave/math.sty", package = "kfbmisc")
    )
  }
  pkg_tex <- ifelse(
    grepl("\\usepackage", packages),
    packages,
    sprintf("\\usepackage{%s}", fs::path_ext_remove(packages))
  )
  if (is.null(pre_begin_document)) {
    pre_begin_document <- ""
  }

  standalone_str <- c(
    "\\documentclass[margin={0mm 0mm 0mm 0mm}]{standalone}",
    pkg_tex,
    pre_begin_document,
    "\\begin{document}",
    read_utf8(
      here::here(output_dir, fs::path_ext_set(tex_base, ".tex"))
    ),
    "\\end{document}"
  )

  # compile in temp
  compile_temp_dir <- fs::path_temp(tex_base)
  fs::dir_create(compile_temp_dir)

  # Write tex file into temp dir
  temp_tex <- fs::path(compile_temp_dir, tex_base)
  write_utf8(standalone_str, temp_tex)

  # look for `tex_base`_ras#.png to copy
  ras_regex <- sprintf("%s_ras[0-9]+.png", tex_base)
  raster_images <- fs::dir_ls(path = output_dir, regexp = ras_regex)
  if (length(raster_images) > 0) {
    fs::file_copy(raster_images, compile_temp_dir, overwrite = TRUE)
  }

  # This is a bit of a "hack" to prevent an issue with latexmk looking for bbl
  temp_bbl <- fs::path(compile_temp_dir, fs::path_ext_set(tex_base, "bbl"))
  fs::file_create(temp_bbl)

  # `latexmk`
  # https://texdoc.org/serve/latexmk/0
  system(
    glue::glue(r'(
      cd "{compile_temp_dir}" &&
      latexmk -pdf -interaction=nonstopmode -bibtex- -quiet -jobname={tex_base} {temp_tex}
    )'),
    ignore.stdout = TRUE
  )

  # Copy file to filename
  pdf_name <- fs::path_ext_set(tex_base, "pdf")
  fs::file_copy(
    fs::path(compile_temp_dir, pdf_name), fs::path(output_dir),
    overwrite = TRUE
  )

  # Delete
  fs::dir_delete(compile_temp_dir)

  # Conditionally create png as well
  if (create_png == TRUE) {
    png_name <- fs::path_ext_set(tex_base, "png")
    system(
      glue::glue(r'(
        cd "{here::here(output_dir)}" &&
        magick -density 300 {pdf_name} {png_name}
      )'),
      ignore.stdout = TRUE
    )
  }

  return(invisible(TRUE))
}



# Taken directly from xfun
read_utf8 <- function(con, error = FALSE) {
  opts <- options(encoding = "native.enc")
  on.exit(options(opts), add = TRUE)
  x <- readLines(con, encoding = "UTF-8", warn = FALSE)
  x
}
write_utf8 <- function(text, con, ...) {
  if (is.null(text)) {
    text <- character(0)
  }
  if (identical(con, "")) {
    cat(text, sep = "\n", file = con)
  } else {
    opts <- options(encoding = "native.enc")
    on.exit(options(opts), add = TRUE)
    writeLines(enc2utf8(text), con, ..., useBytes = TRUE)
  }
  invisible(con)
}
