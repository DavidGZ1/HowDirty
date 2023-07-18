#' Check if samples are included in both results and annotation
#'
#' This function checks if all samples in the provided results and annotation are present in both. If one sample is misssing in one, it will not be further processed.
#'
#' @param df_conta dataframe containing contaminant information.
#' @param df_samples_annot dataframe containing sample annotation.
#'
#' @return None
#'
#' @examples
#' check_samples_in_results(df_conta, df_samples_annot)
#'
#' @export
check_samples_in_results <- function(df_conta, df_samples_annot){
  #Check if all samples are included in both results and annotation file
  reps_samples_annot <- unique(df_samples_annot$ReplicateName)
  reps_conta_reps <- unique(df_conta$ReplicateName)

  reps_only_samples_annot <- as.character(reps_samples_annot[!reps_samples_annot%in%reps_conta_reps])
  reps_only_conta <- as.character(reps_conta_reps[!reps_conta_reps%in%reps_samples_annot])
  reps_only_conta <- reps_only_conta[!is.na(reps_only_conta)]
  # Check samples
  if(length(reps_only_conta) > 0) {
    message(paste("Warning, the following samples from the Skyline results\nwere not found in the annotation file and won't be processed:\n", paste(reps_only_conta, collapse = ", ")))
  }

  if(length(reps_only_samples_annot) > 0) {
    message(paste("Warning, the following samples from the Annotation file\nwere not found in the Skyline results and won't be processed:\n", paste(reps_only_samples_annot, collapse = ", ")))
  }

}
