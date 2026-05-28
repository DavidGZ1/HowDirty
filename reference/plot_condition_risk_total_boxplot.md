# Plot abundance

Plots a boxplot of the abundance for each condition

## Usage

``` r
plot_condition_risk_total_boxplot(
  input_conta_summ_sample,
  scale = "linear",
  compare_means = TRUE,
  method = "wilcox.test"
)
```

## Arguments

- input_conta_summ_sample:

  dataframe containing abundance and condition.

- scale:

  changes the scale to linear or log10; options = c("linear", "log10").

- compare_means:

  flag if means are compared via a statistical test.

- method:

  test used to compare means ("wilcox.test", "anova", "kruskal.test",
  "t.test")

## Value

ggplot object.

## Examples

``` r
if (FALSE) { # \dontrun{
plot_condition_risk_total_boxplot(input_conta_summ_sample, scale = "linear", compare_means = TRUE, method = "wilcox.test")
} # }
```
