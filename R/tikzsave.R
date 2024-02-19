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

  if (is.null(packages)) {
    packages = c(
      system.file("tikzsave/paper.sty", package = "kfbmisc"), 
      system.file("tikzsave/math.sty", package = "kfbmisc")
    )
  }
  pkg_tex = paste0("\\usepackage{", xfun::sans_ext(packages), "}")

  base = basename(filename)
  dir = here::here(dirname(filename))
  fs::dir_create(dir)

  # Make sure to clean-up
  on.exit({
    delete_if_exists(here::here(dir, "temp.tex"))
    for (ext in c(".aux", ".fdb_latexmk", ".flx", ".log", ".xdv", ".fls", ".bbl")) {
      delete_if_exists(
        here::here(dir, fs::path_ext_set(base, ext))
      )
    }
    tikzDicts = fs::dir_ls(dir, regexp="tikzDictionary")
    lapply(tikzDicts, delete_if_exists)
  })

  # right margin is added cause it usually looks cropped too tight to the right
  # source: https://tex.stackexchange.com/questions/138677/why-does-standalone-not-detect-the-tikz-crop-correctly
  WRAPPER <- list(
    c(
      "\\documentclass[margin={0mm 0mm 0mm 0mm}]{standalone}",
      pkg_tex,
      "\\begin{document}"
    ), 
    c("\\end{document}")
  )

  standalone_str <- c(
    WRAPPER[[1]],
    xfun::read_utf8(
      here::here(dir, fs::path_ext_set(base, ".tex"))
    ),
    WRAPPER[[2]]
  )
  xfun::write_utf8(standalone_str, here::here(dir, "temp.tex"))

  # This is a bit of a "hack" to prevent an issue with latexmk looking for bbl
  fs::file_create(
    here::here(dir,  fs::path_ext_set(base, ".bbl"))
  )

  # `latexmk`
  # https://texdoc.org/serve/latexmk/0
  compile_command <- glue::glue(r'(
    cd "{here::here(dir)}" &&
    latexmk -pdf -interaction=nonstopmode -bibtex- -quiet -jobname={xfun::sans_ext(base)} temp.tex
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

  # Conditionally create png as well
  if (create_png == TRUE) {
    pdf_name = fs::path_ext_set(base, "pdf")
    png_name = fs::path_ext_set(base, "png")
    convert_command = glue::glue(r'(
      cd "{here::here(dir)}" &&
      convert -density 400 {pdf_name} {png_name}
    )')
    system(
      convert_command,
      ignore.stdout = TRUE
    )
  }

  return(invisible(TRUE))
}
