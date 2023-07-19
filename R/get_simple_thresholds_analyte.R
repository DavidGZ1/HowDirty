#' Set arbitrary contaminant threshold
#'
#' Sets arbitrary thresholds for the contaminants found in the provided dataframe.
#'
#' @param df_conta dataframe containing contaminants.
#'
#' @return dataframe.
#'
#' @examples
#' get_simple_thresholds_analyte(df_conta)
#'
#' @export
get_simple_thresholds_analyte <-function(df_conta){
  # Set arbitrary thresholds for the contaminants found in df_conta
  df_conta %>%
    select(AnalyteGroup, Analyte, AnalyteFull) %>%
    unique() %>%
    mutate(Tshd_Area_TICA_perc25 = 0.00015,
           Tshd_Area_TICA_perc50 = 0.0005,
           Tshd_Area_TICA_perc75 = 0.0015,
           Tshd_Area_TICA_perc90 = 0.005)
}
