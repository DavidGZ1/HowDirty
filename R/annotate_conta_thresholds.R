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
  require(mgsub)
  df_conta %>%
    left_join(., df_threshold %>%  select(AnalyteFull, starts_with("Tshd")),
              by = "AnalyteFull") %>%
    mutate(RiskLevel = case_when({{ var }} ==0 ~ 0,
                                 {{ var }} < Tshd_Area_TICA_perc25 ~ 1,
                                 (Tshd_Area_TICA_perc25 <= {{ var }} & {{ var }} < Tshd_Area_TICA_perc50) ~ 2,
                                 (Tshd_Area_TICA_perc50 <= {{ var }} & {{ var }} < Tshd_Area_TICA_perc75) ~ 3,
                                 (Tshd_Area_TICA_perc75 <= {{ var }} & {{ var }} < Tshd_Area_TICA_perc90) ~ 4,
                                 (Tshd_Area_TICA_perc90 <= {{ var }}) ~ 5,
                                 TRUE ~ 6),
           Risk = mgsub(RiskLevel, patt=c(0, 1, 2, 3, 4, 5, 6) ,
                        rep = c("0) Not Detected", "1) Very Low", "2) Low", "3) Medium", "4) High",
                                "5) Very High", "6) No threshold in reference"))) %>%
    mutate(across(c(Risk, RiskLevel), ~as.factor(.x))) %>%
    select(-starts_with("Tshd_"))
}
# annotate_conta_thresholds <- function(df_conta, df_threshold){
#   require(mgsub)
#   df_conta %>%
#     left_join(., df_threshold %>%  select(AnalyteFull, starts_with("Tshd")),
#               by = "AnalyteFull") %>%
#     mutate(RiskLevel = case_when(Abundance ==0 ~ 0,
#                                  Abundance < Tshd_Area_TICA_perc25 ~ 1,
#                                  (Tshd_Area_TICA_perc25 <= Abundance & Abundance < Tshd_Area_TICA_perc50) ~ 2,
#                                  (Tshd_Area_TICA_perc50 <= Abundance & Abundance < Tshd_Area_TICA_perc75) ~ 3,
#                                  (Tshd_Area_TICA_perc75 <= Abundance & Abundance < Tshd_Area_TICA_perc90) ~ 4,
#                                  (Tshd_Area_TICA_perc90 <= Abundance) ~ 5,
#                                  TRUE ~ 6),
#            Risk = mgsub(RiskLevel, patt=c(0, 1, 2, 3, 4, 5, 6) ,
#                         rep = c("0) Not Detected", "1) Very Low", "2) Low", "3) Medium", "4) High",
#                                 "5) Very High", "6) No threshold in reference"))) %>%
#     mutate(Risk = as.factor(Risk)) %>%
#     select(-starts_with("Tshd_"))
# }
