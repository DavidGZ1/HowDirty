# Get thresholds for sample file

If no threshold file is provided, thresholds are set using this
function.

## Usage

``` r
get_simple_thresholds_sample(breaks = c(0.83, 1.5, 4.6, 12))
```

## Arguments

- breaks:

  numeric vector of length 4; abundance quantile breakpoints defining
  risk levels 1-4.

## Value

dataframe containing break points and labels.

## Examples

``` r
if (FALSE) { # \dontrun{
get_simple_thresholds_sample()
} # }
```
