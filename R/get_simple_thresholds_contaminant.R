#' Set arbitrary contaminant threshold
#'
#' Sets arbitrary thresholds for the contaminants found in the provided dataframe. Default thresholds are based on the qunatile75 of all conaminants in the reference dataset from two months of analyses
#'
#' @param df_conta dataframe containing contaminants
#' @param breaks numerical list containing default break values.
#'
#' @return dataframe with thresholds
#'
#' @examples
#' get_simple_thresholds_contaminant(df_conta, breaks = c(0.00016, 0.0004, 0.0013, 0.0057))
#'
#' @export
get_simple_thresholds_contaminant <- function(df_conta, breaks = c(0.00016, 0.0004, 0.0013, 0.0057)){
  # Set arbitrary thresholds for the Contaminants found in df_conta
  # default thresholds based on the quantile75 of all contaminants in the reference dataset from two months of analyses
  conta_columns <- c("ContaminantGroup", "Contaminant")
  if(length(breaks) != 4 | !is.numeric(breaks)) stop("breaks must be a numeric vector with four values")
  if(!all(conta_columns%in% names(df_conta))) stop(paste0("df_conta must be a dataframe containing the columns: ", paste0(conta_columns, collapse = ", ")))

  output <- unique(df_conta[, conta_columns])
  output$Tshd_abundance_quantile25 = breaks[1]
  output$Tshd_abundance_quantile50 = breaks[2]
  output$Tshd_abundance_quantile75 = breaks[3]
  output$Tshd_abundance_quantile90 = breaks[4]
  return(output)
}
