#' Generate a HowDirty report programmatically
#'
#' Renders the HowDirty RMarkdown template with all parameters supplied as R
#' arguments, without needing to edit the Rmd header manually.
#'
#' @param dataset Short name for the dataset; used as prefix for output filenames.
#' @param file_peak_areas Path to the Skyline PeakAreas_Contaminants CSV export.
#' @param file_annotation Path to the sample annotation CSV.
#' @param file_ref_thresholds Path to a reference HowDirty Excel output, or FALSE to use simple thresholds.
#' @param output_directory Directory where the Excel results file is saved (default: "results").
#' @param remove_missing_contaminants Logical; remove contaminants undetected in all samples (default: TRUE).
#' @param n_top_contaminant_groups Number of top contaminant groups to show in per-sample plots (default: 10).
#' @param multiply_dilution_factor Logical; multiply Abundance by DilutionFactor column (default: FALSE).
#' @param plots_interactive Logical; TRUE for interactive plotly output, FALSE for static ggplot (default: TRUE).
#' @param output_file Name of the output HTML file. Defaults to \code{"{dataset}_HowDirtyReport.html"}.
#' @param output_dir Directory where the HTML report is written (default: current working directory).
#' @param user_names Optional string identifying the analyst(s).
#' @param notes Optional notes string embedded in the report header.
#'
#' @return Path to the generated HTML report (invisibly).
#'
#' @examples
#' \dontrun{
#' generate_howdirty_report(
#'   dataset            = "Experiment1",
#'   file_peak_areas    = "PeakAreas_Contaminants.csv",
#'   file_annotation    = "samples_annotation.csv",
#'   file_ref_thresholds = FALSE
#' )
#' }
#'
#' @export
generate_howdirty_report <- function(dataset,
                                     file_peak_areas,
                                     file_annotation,
                                     file_ref_thresholds         = FALSE,
                                     output_directory            = "results",
                                     remove_missing_contaminants = TRUE,
                                     n_top_contaminant_groups    = 10,
                                     multiply_dilution_factor    = FALSE,
                                     plots_interactive           = TRUE,
                                     output_file                 = NULL,
                                     output_dir                  = ".",
                                     user_names                  = "",
                                     notes                       = ""){
  if(!file.exists(file_peak_areas))  stop("file_peak_areas not found: ", file_peak_areas)
  if(!file.exists(file_annotation))  stop("file_annotation not found: ", file_annotation)
  if(!identical(file_ref_thresholds, FALSE) && !file.exists(file_ref_thresholds)){
    stop("file_ref_thresholds not found: ", file_ref_thresholds)
  }

  template <- system.file(
    "rmarkdown/templates/howdirty/skeleton/skeleton.Rmd",
    package = "HowDirty"
  )

  if(is.null(output_file)){
    output_file <- paste0(dataset, "_HowDirtyReport.html")
  }

  rmarkdown::render(
    input      = template,
    output_file = output_file,
    output_dir  = output_dir,
    params = list(
      DataSet                    = dataset,
      UserNames                  = user_names,
      Notes                      = notes,
      PeakAreasContaminantsFile  = file_peak_areas,
      AnnotationFile             = file_annotation,
      RefThresholdsFile          = file_ref_thresholds,
      OutputDirectory            = output_directory,
      RemoveMissingContaminants  = remove_missing_contaminants,
      nTopContaminantGroups      = n_top_contaminant_groups,
      MultiplyDilutionFactor     = multiply_dilution_factor,
      PlotsInteractive           = plots_interactive
    ),
    envir = new.env(parent = globalenv())
  )

  invisible(file.path(output_dir, output_file))
}


#' Batch-generate HowDirty reports
#'
#' Renders one HowDirty HTML report per row in a dataset manifest data frame.
#' Each row represents one dataset; failed datasets are reported but do not stop
#' the batch.
#'
#' @param datasets data frame with one row per dataset. Required columns:
#'   \describe{
#'     \item{dataset}{Short name used as filename prefix.}
#'     \item{file_peak_areas}{Path to the Skyline CSV export.}
#'     \item{file_annotation}{Path to the sample annotation CSV.}
#'   }
#'   Optional column: \code{file_ref_thresholds} (defaults to FALSE if absent).
#' @param output_dir Directory where all HTML reports are written (default: current working directory).
#' @param ... Additional arguments passed to \code{\link{generate_howdirty_report}}.
#'
#' @return data frame with columns \code{dataset}, \code{status} ("success" or "error"),
#'   \code{output_file}, and \code{error} (NA on success, message on failure).
#'
#' @examples
#' \dontrun{
#' manifest <- data.frame(
#'   dataset         = c("Exp1", "Exp2"),
#'   file_peak_areas = c("exp1/PeakAreas.csv", "exp2/PeakAreas.csv"),
#'   file_annotation = c("exp1/annotation.csv", "exp2/annotation.csv"),
#'   stringsAsFactors = FALSE
#' )
#' run_howdirty_batch(manifest, output_dir = "reports")
#' }
#'
#' @export
run_howdirty_batch <- function(datasets, output_dir = ".", ...){
  required_cols <- c("dataset", "file_peak_areas", "file_annotation")
  missing_cols  <- setdiff(required_cols, names(datasets))
  if(length(missing_cols) > 0){
    stop("datasets is missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  if(!"file_ref_thresholds" %in% names(datasets)){
    datasets$file_ref_thresholds <- FALSE
  }

  n <- nrow(datasets)
  results <- vector("list", n)

  for(i in seq_len(n)){
    ds   <- datasets$dataset[i]
    out  <- paste0(ds, "_HowDirtyReport.html")
    message(sprintf("[%d/%d] Processing: %s", i, n, ds))
    tryCatch({
      generate_howdirty_report(
        dataset             = ds,
        file_peak_areas     = datasets$file_peak_areas[i],
        file_annotation     = datasets$file_annotation[i],
        file_ref_thresholds = datasets$file_ref_thresholds[i],
        output_file         = out,
        output_dir          = output_dir,
        ...
      )
      results[[i]] <- list(dataset = ds, status = "success",
                           output_file = file.path(output_dir, out), error = NA)
    }, error = function(e){
      warning(sprintf("Failed [%s]: %s", ds, conditionMessage(e)))
      results[[i]] <<- list(dataset = ds, status = "error",
                            output_file = NA, error = conditionMessage(e))
    })
  }

  do.call(rbind, lapply(results, as.data.frame, stringsAsFactors = FALSE))
}
