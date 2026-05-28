# Plot total abundance

Plots the total abundance vs replicate name

## Usage

``` r
plot_sample_risk_total(
  input_conta_summ_sample,
  order_x = "Sample",
  scale = "linear"
)
```

## Arguments

- input_conta_summ_sample:

  dataframe containing the total abundance values as well as sample
  names.

- order_x:

  column used for the x-axis order.

- scale:

  changes the scale to linear or log10; options = c("linear", "log10").

## Value

ggplot object.

## Examples

``` r
if (FALSE) { # \dontrun{
plot_sample_risk_total(input_conta_summ_sample, order_x = "Sample", scale = "linear")
} # }
```
