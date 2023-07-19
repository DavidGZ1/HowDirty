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
annotate_conta_thresholds_samples <- function(df_conta_summ_sample, df_threshold_sample){
  require(mgsub)
  df_conta_summ_sample %>%
    mutate(Risk =
             cut(x = Abundance_total,
                 breaks = df_threshold_sample$Breaks,
                 labels = df_threshold_sample$Labels[-1]))  %>%
    drop_na() %>%
    mutate(RiskLevel= as.numeric(substr(as.character(Risk),1,1)),
           RiskLevel = factor(RiskLevel, levels = c(0:6))) %>%
    relocate(Condition, Sample, ReplicateName, Abundance_total, RiskLevel, Risk)
}
