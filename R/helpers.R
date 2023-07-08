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
count_factor_columns <- function(df){
  df  %>%
    summarise(across(where(is.factor), ~n_distinct(.x))) %>%
    t() %>%
    as.data.frame() %>%
    rename("Count" = 1) %>%
    rownames_to_column("Variable")
}

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
get_h <- function() {
  # Get the current figure height in pixels
  # solution to print ggplotly inside asis from https://stackoverflow.com/questions/61906480/how-to-display-ggplotly-plots-with-dynamically-created-tabs-and-for-loops
  with(knitr::opts_current$get(c("fig.height", "dpi", "fig.retina")),
       fig.height*dpi/fig.retina)
}
