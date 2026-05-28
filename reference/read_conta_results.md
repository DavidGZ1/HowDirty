# Read Conta Results

This function reads the contaminants provided by Skyline and renames the
contaminant groups.

## Usage

``` r
read_conta_results(file_report_skyline, simplify_ContaminantGroup = TRUE)
```

## Arguments

- file_report_skyline:

  The file containing the Skyline report.

- simplify_ContaminantGroup:

  Flag if contaminant names should be simplified.

## Value

dataframe with the transformed input.

## Examples

``` r
if (FALSE) { # \dontrun{
read_conta_results("PeakAreas_Contaminants.csv")
} # }
```
