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
get_simple_thresholds_contaminant <-function(df_conta){
  # Set arbitrary thresholds for the Contaminants found in df_conta
  df_conta %>%
    select(ContaminantGroup, Contaminant) %>%
    unique() %>%
    mutate(Tshd_abundance_quantile25 = 0.00015,
           Tshd_abundance_quantile50 = 0.0005,
           Tshd_abundance_quantile75 = 0.0015,
           Tshd_abundance_quantile90 = 0.005)
}
