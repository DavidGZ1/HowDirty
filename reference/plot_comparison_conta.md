# Plot contamination comparison across datasets

Boxplot of total contamination abundance per dataset. Useful for
comparing contamination levels across instruments, batches, or time
periods.

## Usage

``` r
plot_comparison_conta(df_combined, scale = "linear", compare_means = FALSE)
```

## Arguments

- df_combined:

  combined dataframe from
  [`compare_howdirty`](https://davidgz1.github.io/HowDirty/reference/compare_howdirty.md).

- scale:

  "linear" or "log10" for the y-axis (default: "linear").

- compare_means:

  logical; add Wilcoxon pairwise comparison (default: FALSE).

## Value

ggplot object.

## Examples

``` r
if (FALSE) { # \dontrun{
plot_comparison_conta(combined)
plot_comparison_conta(combined, scale = "log10", compare_means = TRUE)
} # }
```
