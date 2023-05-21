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
get_simple_thresholds_analyte <-function(df_conta){
  # Set arbitrary thresholds for the analytes found in df_conta
  df_conta %>%
    select(AnalyteGroup, Analyte, AnalyteFull) %>%
    unique() %>%
    mutate(Tshd_Area_TICA_perc25 = 0.00015,
           Tshd_Area_TICA_perc50 = 0.0005,
           Tshd_Area_TICA_perc75 = 0.0015,
           Tshd_Area_TICA_perc90 = 0.005)
}
