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
#' bmi3(bmi.vals)
#'
#' @export
get_simple_thresholds_sample <-function(breaks = c(0, 0.05, 0.1, 0.55, 1.0, Inf)){
  # Set arbitrary thresholds for the samples
  data.frame(Breaks = breaks,
             # Sum = c(0, 0.5, 1.0, 5, 10, Inf), #TODO remove if ok
             Labels = c("0) ERROR", "1) Very Low (OK)", "2) Low (OK)", "3) Medium (Warning)",
                        "4) High (DO NOT PROCEED)", "5) Very High (DO NOT PROCEED)"))
}
