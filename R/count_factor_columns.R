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
