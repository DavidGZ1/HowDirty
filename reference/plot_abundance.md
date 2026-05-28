# Plot abundance

Plots the abundance of contaminants as a boxplot

## Usage

``` r
plot_abundance(input_conta, level, variable, scale = "linear")
```

## Arguments

- input_conta:

  dataframe containing contaminants and abundances.

- level:

  name of the column containing x-values.

- variable:

  name of the column containing y-values.

- scale:

  changes the scale to linear or log10; options = c("linear", "log10").

## Value

ggplot object.

## Examples

``` r
if (FALSE) { # \dontrun{
plot_abundance(input_conta, level, variable, scale = "linear")
} # }
```
