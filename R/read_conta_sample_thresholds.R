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
           Labels = c("0) ERROR",
                      "1) Very Low (OK)",
                      "2) Low (OK)",
                      "3) Medium (Warning)",
                      "4) High (Warning)",
                      "5) Very High (DO NOT PROCEED)")) %>%
    as.data.frame()
  return(output)
}
