# Plot pseudochromatogram

Plots the Abundance vs. Retention time

## Usage

``` r
plot_pseudochromatogram(input_conta, scale = "linear")
```

## Arguments

- input_conta:

  dataframe containing abundance and retention time.

- scale:

  changes the scale to linear or log10; options = c("linear", "log10").

## Value

ggplot object.

## Examples

``` r
if (FALSE) { # \dontrun{
plot_pseudochromatogram(input_conta, scale = "linear")
} # }
```
