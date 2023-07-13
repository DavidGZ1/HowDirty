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
annotate_conta_thresholds <- function(df_conta, df_threshold, var){
  # if only the ContaminantGroup is present, calculate for ContaminantGroup
  require(mgsub)
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
             Risk = mgsub(RiskLevel, patt=c(0, 1, 2, 3, 4, 5, 6) ,
                          rep = c("0) Not Detected",
                                  "1) Very Low",
                                  "2) Low",
                                  "3) Medium",
                                  "4) High",
                                  "5) Very High",
                                  "6) No threshold in reference"))) %>%
      mutate(across(c(Risk, RiskLevel), ~as.factor(.x))) %>%
      select(-starts_with("Tshd_"))
  }
  if(!any(c("ContaminantGroup", "Contaminant") %in% names(df_conta))){
    stop("At least one of these variables must be present to assign the risk levels: Contaminant or ContaminantGroup")
  }
  return(output)
}

