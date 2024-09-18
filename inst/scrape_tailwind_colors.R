library(rvest)

html <- read_html("https://tailwindcss.com/docs/customizing-colors")

html |>
  rvest::html_elements(css = "div.\32xl\\:contents")
