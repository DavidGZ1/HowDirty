# Check if all the required columns are present in the Skyline export (df_conta)

This function checks if the required columns are provided in the Skyline
output: "Protein", "Peptide", "Replicate.Name",
"Peptide.Retention.Time", "Total.Area.MS1", "Total.Ion.Current.Area")

## Usage

``` r
check_conta_columns(df_conta, verbose = TRUE)
```

## Arguments

- df_conta:

  dataframe containing contaminant information.

- verbose:

  logical, if TRUE, prints a validation message when all the columns are
  found.

## Value

None

## Examples

``` r
if (FALSE) { # \dontrun{
check_conta_columns(conta)
} # }
```
