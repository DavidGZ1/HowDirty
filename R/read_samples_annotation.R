#' Read Sample Annotation
#'
#' Reads the annotation in the sample input file and does renaming and missing value handling.
#'
#' @param file_samples_annotation Input file containing sample annotation.
#'
#' @return The input file with changed values.
#'
#' @examples
#' read_samples_annotation(file_samples_annotation)
#'
#' @export
read_samples_annotation <- function(file_samples_annotation){
  output <-   read.csv(file_samples_annotation)
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
