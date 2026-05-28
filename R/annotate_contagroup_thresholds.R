#' Annotate thresholds of contaminant groups
#'
#' Annotates the contaminant groups with thresholds
#'
#' @param df_conta dataframe with contaminants.
#' @param df_threshold dataframe with thresholds.
#'
#' @return dataframe containing the input dataframe
#'
#' @examples
#' \dontrun{
#' annotate_contagroup_thresholds(df_conta, thresholds)
#' }
#'
#' @export
annotate_contagroup_thresholds <- function(df_conta, df_threshold){
  # if only the ContaminantGroup is present, calculate for ContaminantGroup
  if(!all((c("Contaminant") %in% names(df_conta))) & ("ContaminantGroup" %in% names(df_conta))){
    message("Thresholds assigned at the ContaminantGroup level (total sum of contaminant abundance)")
    # if only the ContaminantGroup is present, calculate for ContaminantGroup
    df_threshold_total <-  df_threshold %>%
      group_by(ContaminantGroup) %>%
      summarise(across(starts_with("Tshd"), ~sum(.x, na.rm = TRUE)))
    output <-
      df_conta%>%
      left_join(., df_threshold_total %>%  select(ContaminantGroup, starts_with("Tshd")),
                by = c("ContaminantGroup")) %>%
      mutate(RiskLevel = case_when(Abundance_total ==0 ~ 0,
                                   Abundance_total < Tshd_abundance_quantile25 ~ 1,
                                   (Tshd_abundance_quantile25 <= Abundance_total & Abundance_total < Tshd_abundance_quantile50) ~ 2,
                                   (Tshd_abundance_quantile50 <= Abundance_total & Abundance_total < Tshd_abundance_quantile75) ~ 3,
                                   (Tshd_abundance_quantile75 <= Abundance_total & Abundance_total < Tshd_abundance_quantile90) ~ 4,
                                   (Tshd_abundance_quantile90 <= Abundance_total) ~ 5,
                                   TRUE ~ 6),
             Risk = .apply_risk_labels(RiskLevel)) %>%
      mutate(across(c(Risk, RiskLevel), ~as.factor(.x))) %>%
      select(-starts_with("Tshd_"))
  }
  if(!any(c("ContaminantGroup") %in% names(df_conta))){
    stop("At least one of these variables must be present to assign the risk levels: Contaminant or ContaminantGroup")
  }
  return(output)
}
