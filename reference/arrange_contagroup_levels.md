# Arrange contaminant group levels

Arranges the contaminant group levels based on overall median abundance.

## Usage

``` r
arrange_contagroup_levels(df_conta, metric = "median")
```

## Arguments

- df_conta:

  dataframe containing the contaminants.

- metric:

  character; column used to order groups ("min", "median", "max",
  "total").

## Value

dataframe with the arranged contaminant group levels.

## Examples

``` r
if (FALSE) { # \dontrun{
arrange_contagroup_levels(df_conta, metric = "median")
} # }
```
