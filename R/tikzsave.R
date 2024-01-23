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
#' @param ... Passed to `ggplot2::ggsave`
#' 
#' @return Invisibly returns filename
tikzsave <- function(filename, plot = ggplot2::last_plot(), packages = NULL, recompile = TRUE, ...) {

  fs::dir_create(fs::path_dir(filename), recurse = TRUE)
  
  # plot -> tikzpicture
  ggplot2::ggsave(
    filename = filename, plot = plot, 
    device = function(...) tikzDevice::tikz(..., standAlone = FALSE, verbose = FALSE),
    ...
  )
  
  compile_tikzpicture(filename, packages = packages, recompile = recompile)

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
#'
#' @return Invisibly returns `TRUE` if compilation succeeds.
#' @export
compile_tikzpicture <- function(filename, packages = NULL, recompile = TRUE) {

  if (is.null(packages)) {
    packages = c(
      system.file("tikzsave/paper.sty", package = "kfbmisc"), 
      system.file("tikzsave/math.sty", package = "kfbmisc")
    )
  }
  pkg_tex = paste0("\\usepackage{", packages, "}")

  dir = dirname(filename)
  base_san_ext = xfun::sans_ext(basename(filename))
  file_ext = xfun::file_ext(filename)

  # Make sure to clean-up
  on.exit({
    delete_if_exists(here::here(dir, "temp.tex"))
    for (ext in c(".aux", ".fdb_latexmk", ".flx", ".log", ".xdv", ".fls")) {
      delete_if_exists(
        here::here(dir, paste0(base_san_ext, ext))
      )
    }
  })

  # Don't recompile new files
  pdf_name <- gsub(".tex", ".pdf", filename)
  if (file.exists(pdf_name)) {
    tikz_compile_time <- file.info(filename)$mtime
    pdf_compile_time <- file.info(pdf_name)$mtime

    if ((pdf_compile_time > tikz_compile_time) & recompile == FALSE) {
      return(invisible(TRUE))
    }
  }

  # right margin is added cause it usually looks cropped too tight to the right
  # source: https://tex.stackexchange.com/questions/138677/why-does-standalone-not-detect-the-tikz-crop-correctly
  WRAPPER <- list(
    c(
      "\\documentclass[margin={0mm 0mm 10mm 0mm}]{standalone}",
      pkg_tex,
      "\\begin{document}"
    ), 
    c("\\end{document}")
  )

  standalone_str <- c(
    WRAPPER[[1]],
    xfun::read_utf8(filename),
    WRAPPER[[2]]
  )
  xfun::write_utf8(standalone_str, here::here(dir, "temp.tex"))

  # `latexmk`
  # https://texdoc.org/serve/latexmk/0
  compile_command <- glue::glue(r'(
    cd "{here::here(dir)}" &&
    latexmk -pdf -interaction=nonstopmode -bibtex- -jobname={base_san_ext} temp.tex
  )')
  system(
    compile_command,
    ignore.stdout = TRUE
  )

  cleanup_command <- glue::glue(r'(
    cd "{here::here(dir)}" && latexmk -c
  )')
  system(
    cleanup_command,
    ignore.stdout = TRUE
  )

  return(invisible(TRUE))
}
