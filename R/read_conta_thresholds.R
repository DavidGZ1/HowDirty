#' Read contaminant thresholds
#'
#' Reads the contaminant thresholds from a reference dataset and annotates a dataset which did not provide a threshold file
#'
#' @param file_report_howdirty file name of another HowDirty output that inlcudes thresholds.
#'
#' @return dataframe containing the contaminant thresholds.
#'
#' @examples
#' read_conta_thresholds(file_report_howdirty)
#'
#' @export
read_conta_thresholds <- function(file_report_howdirty){
  # read the HowDirty Excel report file from a reference dataset and get the thresholds (contaminant level) to annotate another dataset
  output <-
    readxl::read_xlsx(file_report_howdirty, sheet = "conta_summ_contaminant") %>%
    filter(Abundance_total != 0) %>%
    select(ContaminantGroup, Contaminant,
           Abundance_quantile25, Abundance_quantile50 = Abundance_median,
           Abundance_quantile75, Abundance_quantile90) %>%
    rename_with(~gsub("Abundance_quantile", "Tshd_abundance_quantile", .x)) %>%
    # rename_with(~gsub("Abundance_quantile", "Tshd_Area_TICA_perc", .x)) %>%
    as.data.frame()
  return(output)
}
