#' Arrange contaminant group levels
#'
#' Arranges the contaminant group levels based on overall median abundance.
#'
#' @param df_conta dataframe containing the contaminants.
#'
#' @return dataframe with the arranged contaminant group levels.
#'
#' @examples
#' arrange_contagroup_levels(df_conta, metric = "median")
#'
#' @export
arrange_contagroup_levels <- function(df_conta, metric = "median"){
  #arrange column ContaminantGroup based on metric
  # metric = min, median, max, total
  if(metric %in% c("min", "median", "max", "total")){
    variable <-  paste0("Abundance_", metric)
  }else{
    stop("metric must be any of the following: = min, median, max, total")
  }
  contaminantgroup_arranged <-
    df_conta %>%
    summarize_conta(., ContaminantGroup) %>%
    arrange(desc(get(variable))) %>%
    pull(ContaminantGroup) %>%
    as.character()
  output <- df_conta %>%
    mutate(ContaminantGroup = factor(ContaminantGroup, contaminantgroup_arranged))
  return(output)
}
