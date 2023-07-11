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
