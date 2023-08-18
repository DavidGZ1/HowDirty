#' Annotate contaminated samples
#'
#' Annotates the contamination results using the samples annotation, multiplies by dilution factor, and assigns risc level
#'
#' @param df_conta dataframe containing the contaminated samples.
#' @param df_samples_annot dataframe with annotated samples.
#' @param remove_missing flag if missing values should be removed.
#' @param multiply_dilution_factor flag if multiplication by dilution factor happens.
#'
#' @return merged dataframe containing contaminated and annotated samples.
#'
#' @examples
#' annotate_conta_samples(df_conta, df_samples_annot, remove_missing = FALSE, multiply_dilution_factor = FALSE)
#'
#' @export
annotate_conta_samples <- function(df_conta, df_samples_annot, remove_missing = FALSE, multiply_dilution_factor = FALSE){
  #Merge samples_annot and contamination
  ouptut <- inner_join(df_samples_annot, df_conta, by = "ReplicateName") %>%
    relocate(c("Condition"), "Sample", "ReplicateName", "DilutionFactor", "ContaminantGroup", "Contaminant") %>%
    arrange(Condition)
  if(remove_missing == TRUE){
    ouptut <-
      ouptut %>%
      group_by(ReplicateName, ContaminantGroup, Contaminant) %>%
      filter(!all(TotalAreaMS1 == 0)) %>%
      ungroup() %>%
      droplevels()
  }
  # Multiply by DilutionFactor
  if(multiply_dilution_factor == TRUE){
    ouptut <-  mutate(ouptut, Abundance = signif(Abundance*DilutionFactor, 4))
    message("WARNING: Abundance was multipled by the dilution factor before assigning the RiskLevel")

  }

  return(ouptut)
}
