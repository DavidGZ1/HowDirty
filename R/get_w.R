#' Short description
#'
#' description
#'
#' @param x type of input object (e.g. numeric vector).
#'
#' @return type of output object (e.g. numeric vector).
#'
#' @examples
#' bmi.vals <- rnorm(n = 50, mean = 25, sd = 3)
#' *' bmi3(bmi.vals)
#' #'
#' @export
get_w <- function() {
  # Get the current figure width in pixels
  # solution to print ggplotly inside asis from https://stackoverflow.com/questions/61906480/how-to-display-ggplotly-plots-with-dynamically-created-tabs-and-for-loops
  with(knitr::opts_current$get(c("fig.width", "dpi", "fig.retina")),
       fig.width*dpi/fig.retina)
}
