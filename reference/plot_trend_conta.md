# Longitudinal trend plot of total contamination

Plots total contamination abundance over run order, coloured by
RiskLevel. Rows are treated as ordered runs; preserve the desired
sequence before calling.

## Usage

``` r
plot_trend_conta(
  df_summ_sample,
  x = "ReplicateName",
  add_smooth = TRUE,
  facet_by = NULL,
  scale = "linear"
)
```

## Arguments

- df_summ_sample:

  per-sample summary dataframe (output of `summarize_conta` at sample
  level with RiskLevel annotated).

- x:

  character; column name to use as x-axis labels (default:
  "ReplicateName").

- add_smooth:

  logical; overlay a loess trend line (default: TRUE).

- facet_by:

  optional character; column name to facet by (e.g. "Condition").

- scale:

  "linear" or "log10" for the y-axis (default: "linear").

## Value

ggplot object.

## Examples

``` r
if (FALSE) { # \dontrun{
plot_trend_conta(conta_summ_sample)
plot_trend_conta(conta_summ_sample, x = "Sample", facet_by = "Condition")
} # }
```
