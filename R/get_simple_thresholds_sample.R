#' Get thresholds for sample file
#'
#' If no threshold file is provided, thresholds are set using this function.
#'
#' @param None
#'
#' @return dataframe containing break points and labels.
#'
#' @examples
#' get_simple_thresholds_sample()
#'
#' @export
get_simple_thresholds_sample <-function(breaks = c(0.83, 1.50, 4.60, 12)){
  # Set arbitrary thresholds for the samples
  # default thresholds based on reference dataset from two monts of analyses
  if(length(breaks) != 4 | !is.numeric(breaks)) stop("breaks must be a numeric vector with four values")
  breaks = c(0, breaks, Inf)
  data.frame(Breaks = breaks,
             # Sum = c(0, 0.5, 1.0, 5, 10, Inf), #TODO remove if ok
             Labels = c("0) Not Detected",
                        "1) Very Low",
                        "2) Low",
                        "3) Medium",
                        "4) High",
                        "5) Very High"))
}
