# Plot contaminant vs condition

Plots contaminant vs condition, with colour in function of risk and size
in function of median abundance

## Usage

``` r
plot_condition_risk_contaminant(
  input_conta_summ_sample_risk,
  order_x = "Condition",
  order_y = "Abundance",
  show_zeros = FALSE
)
```

## Arguments

- input_conta_summ_sample_risk:

  dataframe containing the contaminant abundance values as well as the
  condition.

- order_x:

  variable name for the order of x-axis.

- order_y:

  variable name for the order of y-axis

- show_zeros:

  flag if zero values should be removed.

## Value

ggplot object.

## Examples

``` r
if (FALSE) { # \dontrun{
plot_condition_risk_contaminant(input_conta_summ_sample_risk, order_x = "Condition", order_y = "Abundance", show_zeros = FALSE)
} # }
```
