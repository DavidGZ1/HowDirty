#' Annotate contaminant
#'
#' Annotates the contaminants with risk level thresholds
#'
#' @param df_conta_summ_sample dataframe containing the contaminant values.
#' @param df_threshold_sample dataframe containing thresholds of risk level.
#'
#' @return dataframe with annotated thresholds.
#'
#' @examples
#' annotate_conta_tresholds_samples(df_conta_summ_sample, df_threshold_sample)
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
    # select(Condition, Sample, ReplicateName, TotalAbundance = Abundance_total, Risk) %>%
    # arrange(Condition, Sample, ReplicateName)
    mutate(RiskLevel= as.numeric(substr(as.character(Risk),1,1)),
           RiskLevel = factor(RiskLevel, levels = c(0:6))) %>%
    relocate(Condition, Sample, ReplicateName, Abundance_total, RiskLevel, Risk)
}
