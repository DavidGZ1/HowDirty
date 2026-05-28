#' Short description
#'
#' description
#'
#' @return numeric; figure height in pixels for the current knitr chunk.
#'
#' @examples
#' \dontrun{
#' get_h()
#' }
#' @export
get_h <- function() {
  # Get the current figure height in pixels
  # solution to print ggplotly inside asis from https://stackoverflow.com/questions/61906480/how-to-display-ggplotly-plots-with-dynamically-created-tabs-and-for-loops
  with(knitr::opts_current$get(c("fig.height", "dpi", "fig.retina")),
       fig.height*dpi/fig.retina)
}
