# Generate a HowDirty report programmatically

Renders the HowDirty RMarkdown template with all parameters supplied as
R arguments, without needing to edit the Rmd header manually.

## Usage

``` r
generate_howdirty_report(
  dataset,
  file_peak_areas,
  file_annotation,
  file_ref_thresholds = FALSE,
  output_directory = "results",
  remove_missing_contaminants = TRUE,
  n_top_contaminant_groups = 10,
  multiply_dilution_factor = FALSE,
  plots_interactive = TRUE,
  output_file = NULL,
  output_dir = ".",
  user_names = "",
  notes = ""
)
```

## Arguments

- dataset:

  Short name for the dataset; used as prefix for output filenames.

- file_peak_areas:

  Path to the Skyline PeakAreas_Contaminants CSV export.

- file_annotation:

  Path to the sample annotation CSV.

- file_ref_thresholds:

  Path to a reference HowDirty Excel output, or FALSE to use simple
  thresholds.

- output_directory:

  Directory where the Excel results file is saved (default: "results").

- remove_missing_contaminants:

  Logical; remove contaminants undetected in all samples (default:
  TRUE).

- n_top_contaminant_groups:

  Number of top contaminant groups to show in per-sample plots (default:
  10).

- multiply_dilution_factor:

  Logical; multiply Abundance by DilutionFactor column (default: FALSE).

- plots_interactive:

  Logical; TRUE for interactive plotly output, FALSE for static ggplot
  (default: TRUE).

- output_file:

  Name of the output HTML file. Defaults to
  `"{dataset}_HowDirtyReport.html"`.

- output_dir:

  Directory where the HTML report is written (default: current working
  directory).

- user_names:

  Optional string identifying the analyst(s).

- notes:

  Optional notes string embedded in the report header.

## Value

Path to the generated HTML report (invisibly).

## Examples

``` r
if (FALSE) { # \dontrun{
generate_howdirty_report(
  dataset            = "Experiment1",
  file_peak_areas    = "PeakAreas_Contaminants.csv",
  file_annotation    = "samples_annotation.csv",
  file_ref_thresholds = FALSE
)
} # }
```
