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
annotate_conta_samples <- function(df_conta, df_samples_annot, remove_missing = FALSE, multiply_dilution_factor = FALSE){
  #Merge samples_annot and contamination
  ouptut <- inner_join(df_samples_annot, df_conta, by = "ReplicateName") %>%
    relocate(c("Condition"), "Sample", "ReplicateName", "DilutionFactor", "AnalyteGroup", "Analyte", "AnalyteFull") %>%
    arrange(Condition, AnalyteFull)
  if(remove_missing == TRUE){
    ouptut <-
      ouptut %>%
      group_by(ReplicateName, AnalyteGroup, Analyte, AnalyteFull) %>%
      filter(!all(Height == 0)) %>%
      ungroup()
  }
  # Multiply by DilutionFactor
  if(multiply_dilution_factor == TRUE){
    ouptut <-  mutate(ouptut, Abundance = signif(Abundance*DilutionFactor, 4))
    message("WARNING: Abundance was multipled by the dilution factor before assigning the RiskLevel")

  }

  return(ouptut)
}
