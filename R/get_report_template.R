#' Get the HowDirtyReport template
#'
#' This is a wrapper using rmarkdown::draft to get the HowDirtyReport template copied in the working directory
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
get_report_template <- function(file = "HowDirtyReportTemplate", edit = TRUE, create_dir = FALSE) {
  rmarkdown::draft(file = file,
                   template = "HowDirty",
                   edit = edit,
                   package = "HowDirty",
                   create_dir = create_dir)
}

