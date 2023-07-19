#' Annotate thresholds of contaminant groups
#'
#' description
#'
#' @param df_conta dataframe with contaminants.
#' @param df_threshold dataframe with thresholds.
#' @param var column name used for calculations.
#'
#' @return dataframe containing the input dataframe
#'
#' @examples
#' annotate_contagroup_thresholds(df_conta, Abundance_median)
#'
#' @export
annotate_contagroup_thresholds <- function(df_conta, df_threshold){
  # if only the ContaminantGroup is present, calculate for ContaminantGroup
  require(mgsub)
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
             Risk = mgsub(RiskLevel, patt=c(0, 1, 2, 3, 4, 5, 6) ,
                          rep = c("0) Not Detected", "1) Very Low", "2) Low", "3) Medium", "4) High",
                                  "5) Very High", "6) No threshold in reference"))) %>%
      mutate(across(c(Risk, RiskLevel), ~as.factor(.x))) %>%
      select(-starts_with("Tshd_"))
  }
  if(!any(c("ContaminantGroup") %in% names(df_conta))){
    stop("At least one of these variables must be present to assign the risk levels: Contaminant or ContaminantGroup")
  }
  return(output)
}
