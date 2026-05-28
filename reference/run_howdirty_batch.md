# Batch-generate HowDirty reports

Renders one HowDirty HTML report per row in a dataset manifest data
frame. Each row represents one dataset; failed datasets are reported but
do not stop the batch.

## Usage

``` r
run_howdirty_batch(datasets, output_dir = ".", ...)
```

## Arguments

- datasets:

  data frame with one row per dataset. Required columns:

  dataset

  :   Short name used as filename prefix.

  file_peak_areas

  :   Path to the Skyline CSV export.

  file_annotation

  :   Path to the sample annotation CSV.

  Optional column: `file_ref_thresholds` (defaults to FALSE if absent).

- output_dir:

  Directory where all HTML reports are written (default: current working
  directory).

- ...:

  Additional arguments passed to
  [`generate_howdirty_report`](https://davidgz1.github.io/HowDirty/reference/generate_howdirty_report.md).

## Value

data frame with columns `dataset`, `status` ("success" or "error"),
`output_file`, and `error` (NA on success, message on failure).

## Examples

``` r
if (FALSE) { # \dontrun{
manifest <- data.frame(
  dataset         = c("Exp1", "Exp2"),
  file_peak_areas = c("exp1/PeakAreas.csv", "exp2/PeakAreas.csv"),
  file_annotation = c("exp1/annotation.csv", "exp2/annotation.csv"),
  stringsAsFactors = FALSE
)
run_howdirty_batch(manifest, output_dir = "reports")
} # }
```
