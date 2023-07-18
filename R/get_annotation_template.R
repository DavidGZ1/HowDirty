#' Create a dataframe with the columns required for the annotation file
#' This function creates a dataframe with the columns required for the annotation file.
#' If a file_report_skyline is provided, the ReplicateNameSkyline and Sample columns are populated with unique(input$Replicate.Name).
#'
#' @param file_report_skyline The file containing the Skyline report.
#' @param save logical, if TRUE, saves the samples_annotation_template.csv at the working directory
#'
#' @return None
#'
#' @examples
#' get_annotation_template()
#'
#' @export
get_annotation_template <-function(file_report_skyline = NULL, save = TRUE){
  if(is.null(file_report_skyline)){
    annot_template <- data.frame(ReplicateNameSkyline = c("Example"),
                                 Sample = c("Example"),
                                 Condition = c("Example"),
                                 DilutionFactor = 1)
  }
  if(is.character(file_report_skyline)){
    df_conta_tmp <- read.csv(file_report_skyline)
    check_conta_columns(df_conta_tmp, verbose = FALSE)
    replicate_names <- unique(df_conta_tmp$Replicate.Name)
    rm(df_conta_tmp)
    annot_template <- data.frame(ReplicateNameSkyline = replicate_names,
                                 Sample = replicate_names,
                                 Condition = rep("All_samples", length(replicate_names)),
                                 DilutionFactor = rep(1, length(replicate_names)))
    message("ReplicateNameSkyline names obtained from the input. Please update Sample and Condition columns.")
  }
  if(save == TRUE){
    write.csv(annot_template, file = "samples_annotation_template.csv", row.names = FALSE)
    message(paste("samples_annotation_template.csv saved to ", getwd()))
  }
  return(annot_template)
}





