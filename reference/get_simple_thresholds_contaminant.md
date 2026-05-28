# Set arbitrary contaminant threshold

Sets arbitrary thresholds for the contaminants found in the provided
dataframe. Default thresholds are based on the qunatile75 of all
conaminants in the reference dataset from two months of analyses

## Usage

``` r
get_simple_thresholds_contaminant(
  df_conta,
  breaks = c(0.00016, 4e-04, 0.0013, 0.0057)
)
```

## Arguments

- df_conta:

  dataframe containing contaminants

- breaks:

  numerical list containing default break values.

## Value

dataframe with thresholds

## Examples

``` r
if (FALSE) { # \dontrun{
get_simple_thresholds_contaminant(df_conta, breaks = c(0.00016, 0.0004, 0.0013, 0.0057))
} # }
```
