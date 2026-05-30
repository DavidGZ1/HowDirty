library(HowDirty)

get_template       <- as.logical(Sys.getenv("GET_TEMPLATE",       "FALSE"))
dataset            <- Sys.getenv("DATASET")
file_peak_areas    <- Sys.getenv("FILE_PEAK_AREAS")
file_annotation    <- Sys.getenv("FILE_ANNOTATION")
file_ref           <- Sys.getenv("FILE_REF_THRESHOLDS", "FALSE")
output_dir         <- Sys.getenv("OUTPUT_DIR",          "/data/results")
plots_interactive  <- as.logical(Sys.getenv("PLOTS_INTERACTIVE",          "FALSE"))
user_names         <- Sys.getenv("USER_NAMES", "")
notes              <- Sys.getenv("NOTES",      "")
remove_missing     <- as.logical(Sys.getenv("REMOVE_MISSING_CONTAMINANTS", "TRUE"))
n_top              <- as.integer(Sys.getenv("N_TOP_CONTAMINANT_GROUPS",    "10"))
multiply_df        <- as.logical(Sys.getenv("MULTIPLY_DILUTION_FACTOR",    "FALSE"))

if (!file.exists(file_peak_areas)) {
  stop("Peak areas file not found: ", file_peak_areas)
}

if (get_template) {
  message("Generating annotation template from: ", file_peak_areas)
  get_annotation_template(
    file_report_skyline = file_peak_areas,
    save      = TRUE,
    overwrite = TRUE
  )
  message("Template saved to: ", getwd(), "/samples_annotation_template.csv")
} else {
  if (!file.exists(file_annotation)) {
    stop("Annotation file not found: ", file_annotation)
  }
  if (file_ref != "FALSE" && !file.exists(file_ref)) {
    stop("Reference thresholds file not found: ", file_ref)
  }
  if (file_ref == "FALSE") file_ref <- FALSE

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  message("Generating HowDirty report for dataset: ", dataset)
  generate_howdirty_report(
    dataset                      = dataset,
    file_peak_areas              = file_peak_areas,
    file_annotation              = file_annotation,
    file_ref_thresholds          = file_ref,
    output_directory             = output_dir,
    plots_interactive            = plots_interactive,
    output_dir                   = output_dir,
    user_names                   = user_names,
    notes                        = notes,
    remove_missing_contaminants  = remove_missing,
    n_top_contaminant_groups     = n_top,
    multiply_dilution_factor     = multiply_df
  )
  message("Report written to: ", output_dir)
}
