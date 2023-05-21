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
bmi3 <- function(x) {
  bmi.groups <- cut(x, breaks = c(0, 25, 30, Inf), right = FALSE)
  return(bmi.groups)
}
