#' Risk level labels
#'
#' Named character vector mapping integer risk levels (0-6) to human-readable labels.
#' Use this constant when displaying or comparing risk levels to ensure consistency.
#'
#' @export
RISK_LABELS <- c(
  "0" = "0) Not Detected",
  "1" = "1) Very Low",
  "2" = "2) Low",
  "3" = "3) Medium",
  "4" = "4) High",
  "5" = "5) Very High",
  "6" = "6) No threshold in reference"
)

# Internal helper: converts a vector of integer risk levels to label strings.
.apply_risk_labels <- function(risk_level_int) {
  mgsub::mgsub(risk_level_int, patt = c(0, 1, 2, 3, 4, 5, 6), rep = unname(RISK_LABELS))
}
