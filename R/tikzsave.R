# TODO: Figure out tikzDictionary when using Rmarkdown
# TODO: pdf_to_png using `convert -density 300 mydocument.pdf mydocument.png`

delete_if_exists <- function(filename) {
  if (file.exists(filename)) file.remove(filename)
}

#' Save a tikzpicture and then compile to pdf
#' 
#' Uses `ggplot2::ggsave` with `tikzDevice::tikz` to create the tikz figure
#' and then compiles using `latexmk`.
#' 
#' @inheritParams ggplot2::ggsave
#' @param packages Vector of strings. Absolute file paths to `.sty` files to 
#' include in preamble. Default is my `paper.sty` and `math.sty` in 
#' `inst/tikzsave`.
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
tikzsave <- function(filename, plot = ggplot2::last_plot(), packages = NULL, recompile = TRUE, create_png = FALSE, quiet = TRUE, ...) {

  base = basename(filename)
  dir = here::here(dirname(filename))
  fs::dir_create(dir)
  
  # Replace `.pdf` with `.tex`
  filename = fs::path_ext_set(here::here(dir, base), ".tex")
  
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
    filename, packages = packages, recompile = recompile, 
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
#' @param packages Vector of strings. Absolute file paths to `.sty` files to 
#' include in preamble. Default is my `paper.sty` and `math.sty` in 
#' `inst/tikzsave`.
#' @param recompile Logical. If FALSE, does not update if pdf date
#' is newer than the tex date.
#' @param create_png Logical. If true, will also output a png in addition to 
#' the pdf.
#'
#' @return Invisibly returns `TRUE` if compilation succeeds.
#' 
#' @export
compile_tikzpicture <- function(filename, packages = NULL, recompile = TRUE, create_png = FALSE) {

  base = basename(filename)
  dir = here::here(dirname(filename))
  fs::dir_create(dir)

  if (is.null(packages)) {
    packages = c(
      system.file("tikzsave/paper.sty", package = "kfbmisc"), 
      system.file("tikzsave/math.sty", package = "kfbmisc")
    )
  }
  pkg_tex = paste0("\\usepackage{", xfun::sans_ext(packages), "}")
  
  standalone_str <- c(
    "\\documentclass[margin={0mm 0mm 0mm 0mm}]{standalone}",
    pkg_tex, 
    "\\begin{document}",
    xfun::read_utf8(
      here::here(dir, fs::path_ext_set(base, ".tex"))
    ),
    "\\end{document}"
  )

  temp_tex = fs::path(fs::path_temp(), "temp.tex")
  xfun::write_utf8(standalone_str, temp_tex)
  # This is a bit of a "hack" to prevent an issue with latexmk looking for bbl
  temp_bbl = fs::path(fs::path_temp(), fs::path_ext_set(base, "bbl"))
  fs::file_create(temp_bbl)

  # `latexmk`
  # https://texdoc.org/serve/latexmk/0
  system(
    glue::glue(r'(
      cd "{fs::path_temp()}" &&
      latexmk -pdf -interaction=nonstopmode -bibtex- -quiet -jobname={fs::path_ext_remove(base)} {temp_tex}
    )'),
    ignore.stdout = TRUE
  )

  # Copy file to filename
  pdf_name = fs::path_ext_set(base, "pdf")
  fs::file_copy(
    fs::path(fs::path_temp(), pdf_name), fs::path(dir), overwrite = TRUE
  )

  # Conditionally create png as well
  if (create_png == TRUE) {
    png_name = fs::path_ext_set(base, "png")
    system(
      glue::glue(r'(
        cd "{here::here(dir)}" &&
        convert -density 400 {pdf_name} {png_name}
      )'),
      ignore.stdout = TRUE
    )
  }

  return(invisible(TRUE))
}
