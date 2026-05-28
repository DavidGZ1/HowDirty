# Annotate the threshold for contaminants

Assigns the risk level

## Usage

``` r
annotate_conta_thresholds(df_conta, df_threshold, var)
```

## Arguments

- df_conta:

  dataframe containing the contaminants.

- df_threshold:

  dataframe containing the threshold information.

- var:

  Abundance.

## Value

dataframe containing the risk level.

## Examples

``` r
if (FALSE) { # \dontrun{
annotate_conta_thresholds(df_conta, ref_conta_tshd, Abundance)
} # }
```
