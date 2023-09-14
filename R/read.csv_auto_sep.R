#' Check if all the required columns are present in the Skyline export (df_conta)
#'
#' read.csv using sep = "," or sep = ";" (read.csv) in function of the header
#'
#' @param file the name of the file which the data are to be read from. Each row of the table appears as one line of the file. If it does not contain an absolute path, the file name is relative to the current working directory, getwd(). Tilde-expansion is performed where supported. This can be a compressed file (see file).
#' @param ... Further arguments to be passed to read.csv or read.csv2.
#'
#' @return None
#'
#' @examples
#' read.csv_auto_sep(PeakAreas_Contaminants.csv)
#'
#' @export
read.csv_auto_sep <- function(file, ...){
  # based on solution by G. Grothendieck: https://stackoverflow.com/a/33417611
  header <- readLines(file, n = 1)
  if (grepl(";", header)){
    output <- read.csv2(file, sep = ";", ...)
  }else{
    if (grepl(",", header)){
      output <- read.csv(file, sep = ",", ...)

    }
    else{
      stop("input most be a csv (comma or semicolon separated values) file")
    }
  }
  return(output)
}
