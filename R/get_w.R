#' Short description
#'
#' description
#'
#' @return numeric; figure width in pixels for the current knitr chunk.
#'
#' @examples
#' \dontrun{
#' get_w()
#' }
#' @export
get_w <- function() {
  # Get the current figure width in pixels
  # solution to print ggplotly inside asis from https://stackoverflow.com/questions/61906480/how-to-display-ggplotly-plots-with-dynamically-created-tabs-and-for-loops
  with(knitr::opts_current$get(c("fig.width", "dpi", "fig.retina")),
       fig.width*dpi/fig.retina)
}
