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
