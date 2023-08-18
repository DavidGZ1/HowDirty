#' Get the HowDirtyReport template
#'
#' This is a wrapper using rmarkdown::draft to get the HowDirtyReport template copied in the working directory
#'
#' @param file name of the report file to be copied.
#' @param edit flag.
#' @param create_dir flag if directory should be created.
#'
#' @return None
#'
#' @examples
#' get_report_template(file = "HorDirtyReportTemplate", edit = TRUE, create_dir = FALSE)
#'
#' @export
get_report_template <- function(file = "HowDirtyReportTemplate", edit = FALSE, create_dir = FALSE) {
  rmarkdown::draft(file = file,
                   template = "HowDirty",
                   edit = edit,
                   package = "HowDirty",
                   create_dir = create_dir)
}

