#' Annotate the threshold for contaminants
#'
#' Assigns the risk level
#'
#' @param df_conta dataframe containing the contaminants.
#' @param df_threshold dataframe containing the threshold information.
#' @param var Abundance.
#'
#' @return dataframe containing the risk level.
#'
#' @examples
#' annotate_conta_thresholds(df_conta, ref_conta_tshd, Abundance)
#'
#' @export
annotate_conta_thresholds <- function(df_conta, df_threshold, var){
  # if only the ContaminantGroup is present, calculate for ContaminantGroup
  if("Contaminant" %in% names(df_conta) & "ContaminantGroup" %in% names(df_conta)){
    message("Thresholds assigned at the Contaminant level")
    output <-
      df_conta %>%
      left_join(., df_threshold %>%  select(ContaminantGroup, Contaminant, starts_with("Tshd")),
                by = c("ContaminantGroup", "Contaminant")) %>%
      mutate(RiskLevel = case_when({{ var }} ==0 ~ 0,
                                   {{ var }} < Tshd_abundance_quantile25 ~ 1,
                                   (Tshd_abundance_quantile25 <= {{ var }} & {{ var }} < Tshd_abundance_quantile50) ~ 2,
                                   (Tshd_abundance_quantile50 <= {{ var }} & {{ var }} < Tshd_abundance_quantile75) ~ 3,
                                   (Tshd_abundance_quantile75 <= {{ var }} & {{ var }} < Tshd_abundance_quantile90) ~ 4,
                                   (Tshd_abundance_quantile90 <= {{ var }}) ~ 5,
                                   TRUE ~ 6),
             Risk = .apply_risk_labels(RiskLevel))) %>%
      mutate(across(c(Risk, RiskLevel), ~as.factor(.x))) %>%
      select(-starts_with("Tshd_"))
  }
  if(!any(c("ContaminantGroup", "Contaminant") %in% names(df_conta))){
    stop("At least one of these variables must be present to assign the risk levels: Contaminant or ContaminantGroup")
  }
  return(output)
}

