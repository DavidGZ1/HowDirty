# Compare HowDirty results across multiple datasets

Reads the `conta_summ_sample` sheet from each HowDirty Excel output and
combines them into a single dataframe with a `Dataset` column, ready for
cross-dataset plotting or summary.

## Usage

``` r
compare_howdirty(files)
```

## Arguments

- files:

  named character vector (or named list) mapping dataset labels to
  HowDirty Excel file paths. Names become the `Dataset` column values.

## Value

dataframe combining `conta_summ_sample` from all files, with an added
`Dataset` factor column.

## Examples

``` r
if (FALSE) { # \dontrun{
combined <- compare_howdirty(c(
  Instrument_A = "results/A_report.xlsx",
  Instrument_B = "results/B_report.xlsx"
))
plot_comparison_conta(combined)
} # }
```
