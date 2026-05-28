#' Short description
#'
#' description
#'
#' @param df data frame whose factor columns are counted.
#'
#' @return data frame with one row per factor column and a Count column.
#'
#' @examples
#' \dontrun{
#' count_factor_columns(my_df)
#' }
#' @export
count_factor_columns <- function(df){
  df  %>%
    summarise(across(where(is.factor), ~n_distinct(.x))) %>%
    t() %>%
    as.data.frame() %>%
    rename("Count" = 1) %>%
    rownames_to_column("Variable")
}
