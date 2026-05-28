# Plot contaminant abundance

Plots contaminant vs sample abundance, with colour in function of risk
and size in function of the median abundance

## Usage

``` r
plot_sample_risk_contaminant(
  input_conta_summ_sample_risk,
  order_x = "Sample",
  order_y = "Abundance",
  show_zeros = FALSE
)
```

## Arguments

- input_conta_summ_sample_risk:

  dataframe containing the contamintan abundance and sample names.

- order_x:

  variable name for the order of x-axis.

- order_y:

  variabel name for the order of y-axis

- show_zeros:

  flag if zero values are removed.

## Value

ggplot object.

## Examples

``` r
if (FALSE) { # \dontrun{
plot_sample_risk_contaminant(input_conta_summ_sample_risk, order_x = "Sample", order_y = "Abundance", show_zeros = FALSE)
} # }
```
