#' Check if all the required columns are present in the Skyline export (df_conta)
#'
#' This function checks if the required columns are provided in the Skyline output: "Protein", "Peptide", "Replicate.Name", "Peptide.Retention.Time", "Total.Area.MS1", "Total.Ion.Current.Area")
#'
#' @param df_conta dataframe containing contaminant information.
#' @param verbose logical, if TRUE, prints a validation message when all the columns are found.
#'
#' @return None
#'
#' @examples
#' check_conta_columns(conta)
#'
#' @export
check_conta_columns <- function(df_conta, verbose = TRUE){
  # check if the required columns are present in the Skyline export (df_conta)
  columns_expected <- c("Protein", "Peptide",
                        "Replicate.Name",
                        "Peptide.Retention.Time",
                        "Total.Area.MS1",
                        "Total.Ion.Current.Area")

  columns_found <- columns_expected  %in% names(df_conta)
  columns_expected[!columns_found]

  if(all(columns_found)){
    if(verbose == TRUE) message("OK, the dataframe contains the required columns")
  }else{
    stop(paste("The following columns are missing:",
               paste(columns_expected[!columns_found], collapse = ", ")))
  }
}
