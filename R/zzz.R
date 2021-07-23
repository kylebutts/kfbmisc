.onAttach <- function(libname, pkgname) {
	cli::cli_alert_info("Loading fonts for ggplot theme")
	sysfonts::font_add_google("Fira Sans", "fira_sans")
	sysfonts::font_add_google("Merriweather", "merriweather")
	showtext::showtext_auto()
}
