# Create a dataframe with the columns required for the annotation file This function creates a dataframe with the columns required for the annotation file. If a file_report_skyline is provided, the ReplicateNameSkyline and Sample columns are populated with unique(input\$Replicate.Name).

Extra columns (e.g. Batch, Instrument, Operator) can be added via
`extra_cols` and will be preserved through the full pipeline, making
them available for faceting in plots.

## Usage

``` r
get_annotation_template(
  file_report_skyline = NULL,
  save = TRUE,
  extra_cols = NULL,
  overwrite = FALSE
)
```

## Arguments

- file_report_skyline:

  The file containing the Skyline report.

- save:

  logical, if TRUE, saves the samples_annotation_template.csv at the
  working directory.

- extra_cols:

  character vector of additional metadata column names to include in the
  template (e.g. c("Batch", "Instrument")).

- overwrite:

  logical, if FALSE (default) stops if the output file already exists.

## Value

dataframe with annotation template columns.

## Examples

``` r
if (FALSE) { # \dontrun{
get_annotation_template()
get_annotation_template(extra_cols = c("Batch", "Instrument"))
} # }
```
