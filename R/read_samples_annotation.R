#' Read Sample Annotation
#'
#' Reads the annotation CSV and handles renaming and missing value defaults.
#' Required columns: ReplicateNameSkyline. Optional: Sample, Condition, DilutionFactor.
#' Any additional columns (e.g. Batch, Instrument, Operator) are preserved as-is and
#' carried through the pipeline by \code{annotate_conta_samples()}.
#'
#' @param file_samples_annotation Input file containing sample annotation.
#'
#' @return dataframe with annotation columns; extra metadata columns are retained.
#'
#' @examples
#' read_samples_annotation("samples_annotation.csv")
#'
#' @export
read_samples_annotation <- function(file_samples_annotation){
  output <-   read.csv_auto_sep(file_samples_annotation, na.strings = c("", "#N/A"))
  # fill columns if not included
  if(is.null(output$Sample)) output$Sample = output$ReplicateName
  if(is.null(output$Condition)) output$Condition = "All Samples"
  if(is.null(output$DilutionFactor)) output$DilutionFactor = NA

  output <-
    output %>%
    rename(ReplicateName = ReplicateNameSkyline) %>%
    mutate(across(where(is.character), ~factor(.x, levels = unique(.x)))) %>% # create levels in given order
    mutate(DilutionFactor = replace_na(DilutionFactor, 1))  # complete dilution factor if empty

  return(output)

}
