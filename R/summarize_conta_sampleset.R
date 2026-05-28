#' Summarize the contaminant sample set
#'
#' Summarizes the contaminant sample set to get a summary per sample, per condition, and for all samples
#'
#' @param df_conta dataframe containing the values to summarize.
#' @param df_threshold_sample dataframe containing sample-level thresholds (from read_conta_sample_thresholds or get_simple_thresholds_sample).
#'
#' @return dataframe containing the summaries.
#'
#' @examples
#' \dontrun{
#' summarize_conta_sampleset(df_conta, ref_conta_tshd_sample)
#' }
#'
#' @export
summarize_conta_sampleset <- function(df_conta, df_threshold_sample){
  #get summary per sample
  df_conta_summ_sample <-
    df_conta %>%
    summarize_conta(Condition, Sample, ReplicateName) %>%
    annotate_conta_thresholds_samples(df_threshold_sample)

  df_conta_summ_sampleset <-
    rbind(
      #get summary per condition
      df_conta_summ_sample %>%
        rename(Abundance = Abundance_total) %>% #workaround to use summarize_conta on Abundance_total
        summarize_conta(Condition),
      #get summary for All the samples
      df_conta_summ_sample %>%
        rename(Abundance = Abundance_total) %>%
        mutate(Condition = "All_samples") %>%
        summarize_conta(Condition)
    )
  names(df_conta_summ_sampleset) <- gsub("Abundance_", "Abundance_total_", names(df_conta_summ_sampleset))
  return(df_conta_summ_sampleset)
}
