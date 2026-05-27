#' Create a dataframe with the columns required for the annotation file
#' This function creates a dataframe with the columns required for the annotation file.
#' If a file_report_skyline is provided, the ReplicateNameSkyline and Sample columns are populated with unique(input$Replicate.Name).
#'
#' Extra columns (e.g. Batch, Instrument, Operator) can be added via \code{extra_cols} and
#' will be preserved through the full pipeline, making them available for faceting in plots.
#'
#' @param file_report_skyline The file containing the Skyline report.
#' @param save logical, if TRUE, saves the samples_annotation_template.csv at the working directory.
#' @param extra_cols character vector of additional metadata column names to include in the template (e.g. c("Batch", "Instrument")).
#' @param overwrite logical, if FALSE (default) stops if the output file already exists.
#'
#' @return dataframe with annotation template columns.
#'
#' @examples
#' get_annotation_template()
#' get_annotation_template(extra_cols = c("Batch", "Instrument"))
#'
#' @export
get_annotation_template <- function(file_report_skyline = NULL, save = TRUE,
                                    extra_cols = NULL, overwrite = FALSE){
  if(is.null(file_report_skyline)){
    annot_template <- data.frame(ReplicateNameSkyline = c("Example"),
                                 Sample = c("Example"),
                                 Condition = c("Example"),
                                 DilutionFactor = 1)
  }
  if(is.character(file_report_skyline)){
    df_conta_tmp <- read.csv_auto_sep(file_report_skyline, na.strings = c("", "#N/A"))
    check_conta_columns(df_conta_tmp, verbose = FALSE)
    replicate_names <- unique(df_conta_tmp$Replicate.Name)
    rm(df_conta_tmp)
    annot_template <- data.frame(ReplicateNameSkyline = replicate_names,
                                 Sample = replicate_names,
                                 Condition = rep("All_samples", length(replicate_names)),
                                 DilutionFactor = rep(1, length(replicate_names)))
    message("ReplicateNameSkyline names obtained from the input. Please update Sample and Condition columns.")
  }
  if(!is.null(extra_cols)){
    extra_df <- as.data.frame(
      setNames(replicate(list(rep("Example", nrow(annot_template))), length(extra_cols)),
               extra_cols)
    )
    annot_template <- cbind(annot_template, extra_df)
  }
  if(save == TRUE){
    out_file <- "samples_annotation_template.csv"
    if(!overwrite && file.exists(out_file)){
      stop(out_file, " already exists. Set overwrite = TRUE to replace it.")
    }
    write.csv(annot_template, file = out_file, row.names = FALSE)
    message(paste("samples_annotation_template.csv saved to ", getwd()))
  }
  return(annot_template)
}





