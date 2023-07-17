#' Summarize Contaminant Groups
#'
#' Summarize the data by contaminant group
#'
#' @param df_conta dataframe containing the contaminant groups.
#' @param ... Variables to summarize by.
#'
#' @return dataframe with the summarized contaminant groups.
#'
#' @examples
#' summarize_conta(df_conta, ...)
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
                          quantile90 = ~quantile(.x, probs = c(0.1,0.9))[2],
                          max = ~max(.x, na.rm = TRUE),
                          total = ~sum(.x, na.rm = TRUE))),
              .groups = "drop") %>%
    mutate(across(where(is.numeric), ~signif(.x, 4)))
  return(output)
}
