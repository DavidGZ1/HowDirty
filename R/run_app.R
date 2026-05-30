#' Launch the HowDirty Shiny web app
#'
#' Opens a browser-based GUI for generating contamination reports without
#' writing any R code. Requires the \pkg{shiny} and \pkg{bslib} packages.
#'
#' @param port Port to listen on (default 3838).
#' @param launch.browser Open a browser window automatically
#'   (default \code{TRUE} in interactive sessions).
#'
#' @return Does not return; starts the Shiny server.
#'
#' @examples
#' \dontrun{
#' run_howdirty_app()
#' }
#'
#' @export
run_howdirty_app <- function(port = 3838, launch.browser = interactive()) {
  app_dir <- system.file("shiny", package = "HowDirty")
  if (!nzchar(app_dir)) {
    stop("Shiny app not found. Re-install HowDirty from source.")
  }
  shiny::runApp(app_dir, port = port, launch.browser = launch.browser)
}
