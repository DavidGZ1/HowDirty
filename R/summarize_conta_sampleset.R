#' Summarize the contaminant sample set
#'
#' Summarizes the contaminant sample set to get a summary per sample, per condition, and for all samples
#'
#' @param df_conta dataframe containing the values to summarize.
#'
#' @return dataframe containing the summaries.
#'
#' @examples
#' summarize_conta_sampleset(df_conta)
#'
#' @export
summarize_conta_sampleset <- function(df_conta){
  #get summary per sample
  df_conta_summ_sample <-
    df_conta %>%
    summarize_conta(Condition, Sample, ReplicateName) %>%
    annotate_conta_thresholds_samples(ref_conta_tshd_sample)

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
