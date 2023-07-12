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
