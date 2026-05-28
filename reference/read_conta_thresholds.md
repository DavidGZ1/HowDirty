# Read contaminant thresholds

Reads the contaminant thresholds from a reference dataset and annotates
a dataset which did not provide a threshold file

## Usage

``` r
read_conta_thresholds(file_report_howdirty)
```

## Arguments

- file_report_howdirty:

  file name of another HowDirty output that inlcudes thresholds.

## Value

dataframe containing the contaminant thresholds.

## Examples

``` r
if (FALSE) { # \dontrun{
read_conta_thresholds("reference_report.xlsx")
} # }
```
