# Plot contaminant group risk

Plots contaminant group vs condition/sample, with colour in function of
risk and size in function of Abundance.

## Usage

``` r
plot_contaminantgroup_risk(
  input_conta_summ_contaminantgroup_sample,
  x,
  size,
  order_y = "Abundance",
  show_zeros = FALSE
)
```

## Arguments

- input_conta_summ_contaminantgroup_sample:

  dataframe containing the abundance values as well as condition/sample
  information.

- x:

  values used for x-axis ("Condition", "Sample")

- size:

  any of the abundance measures in dataframe ("Abundance_median",
  "Abundance_total", "Abundance_min", "Abundance_quantile25",
  "Abundance_quantile75", "Abundance_quantile90", "Abundance_max")

- order_y:

  value to order y-axis by ("Abundance", "ContaminantGroup")

- show_zeros:

  logical; if FALSE (default) contaminant groups with all-zero abundance
  are removed

## Value

ggplot object.

## Examples

``` r
if (FALSE) { # \dontrun{
plot_contaminantgroup_risk(input_conta_summ_contaminantgroup_sample, "Condition", "Abundance_median", order_y = "Abundance", show_zeros = FALSE)
} # }
```
