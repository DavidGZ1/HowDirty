# Read thresholds for sample level

Reads the thresholds used for annotation the dataset on sample level
from a reference file if no arbitrary thresholds were set

## Usage

``` r
read_conta_sample_thresholds(file_report_howdirty)
```

## Arguments

- file_report_howdirty:

  path to an already processed HowDirty report excel file containing
  thresholds.

## Value

dataframe with annotated thresholds on sample level.

## Examples

``` r
if (FALSE) { # \dontrun{
read_conta_sample_thresholds("reference_report.xlsx")
} # }
```
