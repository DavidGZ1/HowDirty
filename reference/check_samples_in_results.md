# Check if samples are included in both results and annotation

This function checks if all samples in the provided results and
annotation are present in both. If one sample is misssing in one, it
will not be further processed.

## Usage

``` r
check_samples_in_results(df_conta, df_samples_annot)
```

## Arguments

- df_conta:

  dataframe containing contaminant information.

- df_samples_annot:

  dataframe containing sample annotation.

## Value

None

## Examples

``` r
if (FALSE) { # \dontrun{
check_samples_in_results(df_conta, df_samples_annot)
} # }
```
