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
read_samples_annotation <- function(file_samples_annotation){
  read.csv(file_samples_annotation) %>%
    rename(ReplicateName = ReplicateNameSkyline) %>%
    mutate(across(where(is.character), ~factor(.x, levels = unique(.x)))) %>% # create levels in given order
    mutate(DilutionFactor = replace_na(DilutionFactor, 1))  # complete dilution factor if empty

}
