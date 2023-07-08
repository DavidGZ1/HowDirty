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
summarize_conta <- function(df_conta, ...){
  #summarize results by ...
  output <-
    df_conta %>%
    group_by(...) %>%
    summarise(across(c(Abundance),
                     list(min = ~min(.x, na.rm = TRUE),
                          quantile25 = ~quantile(.x, na.rm = TRUE)[2],
                          median = ~median(.x, na.rm = TRUE),
                          quantile75 = ~quantile(.x, na.rm = TRUE)[4],
                          max = ~max(.x, na.rm = TRUE),
                          total = ~sum(.x, na.rm = TRUE))),
              .groups = "drop") %>%
    mutate(across(where(is.numeric), ~signif(.x, 4)))
  return(output)
}
