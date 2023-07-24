#' Read thresholds for sample level
#'
#' Reads the thresholds used for annotation the dataset on sample level from a reference file if no arbitrary thresholds were set
#'
#' @param file_report_howdirty path to an already processed HowDirty report excel file containing thresholds.
#'
#' @return dataframe with annotated thresholds on sample level.
#'
#' @examples
#' read_conta_sample_thresholds(file_report_howdirty)
#'
#' @export
read_conta_sample_thresholds <- function(file_report_howdirty){
  # read the HowDirty Excel report file from a reference dataset and get the thresholds (sample leve) to annotate another dataset
  output <-
    readxl::read_xlsx(file_report_howdirty, sheet = "conta_summ_sampleset") %>%
    filter(Condition == "All_samples") %>%
    select(-c(Abundance_total_total, Condition)) %>%
    mutate(Abundance_total_zero = 0,
           Abundance_total_inf = Inf) %>%
    pivot_longer(starts_with("Abundance"),
                 names_to = "Quantile",
                 values_to = "Sum",
                 names_prefix = "Abundance_total_") %>%
    arrange(Sum) %>%
    filter(!Quantile %in% c("min", "max")) %>%
    mutate(Breaks =signif(Sum, 2),
           Labels = c("0) Not Detected",
                      "1) Very Low",
                      "2) Low",
                      "3) Medium",
                      "4) High",
                      "5) Very High")) %>%
    as.data.frame()
  return(output)
}
