## diverse helper functions

#' Get string information for Maintainer
#' @param email A character vector for email address to contact maintainer
#' @param github A character vector for github repository to submit issue/feature requests
#' @return A character vector with Maintainer information
#' @export
get_maintainer <- function(email='davidgz.science@gmail.com', github='https://github.com/DavidGZ1/HowDirty'){
  sprintf('%s (%s)\n', email, utils::URLdecode(github))
}


#' Minima without zero
#'
#' Returns the minima of the input values while ignoring zero values.
#'
#' @param x numeric vector.
#'
#' @return numeric vector.
#'
#' @examples
#' min_no_zero(c(0,1,2,3,10))
#'
#' @export
min_no_zero <- function(x, na.rm = TRUE){
  min(x[x != 0], na.rm = na.rm)
}
