#' Run the FPAT Application
#'
#' \code{FPAT} runs the FPAT Shiny Application
#'
#' @references Modified from Deal Attali's code: \url{http://deanattali.com/2015/04/21/r-package-shiny-app/}
#' @importFrom utils install.packages installed.packages
#' @export
FPAT <- function(app="FPAT", ...) {
  temp <- try(class(app), silent=TRUE)
  if (class(temp) == "try-error") app <- deparse(substitute(app))
  Apps <- list.files(system.file("shiny_apps", package = "FPAT"))
  validAppMsg <- paste0("Valid examples are:\n '", paste(Apps, collapse = "', '"), "'")
  appDir <- system.file("shiny_apps", app, package = "FPAT")
  shiny::runApp(appDir, display.mode = "normal",launch.browser = TRUE, ...)
}
