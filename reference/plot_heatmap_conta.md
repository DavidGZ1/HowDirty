# Heatmap of contamination risk

Plots a sample x contaminant heatmap with RiskLevel as fill colour.
Provides an overview of all contaminants across all samples in a single
plot.

## Usage

``` r
plot_heatmap_conta(
  df_conta,
  x = ReplicateName,
  y = Contaminant,
  facet_by = NULL
)
```

## Arguments

- df_conta:

  dataframe containing annotated contaminant results with RiskLevel
  column.

- x:

  column to use on the x-axis (default: ReplicateName).

- y:

  column to use on the y-axis (default: Contaminant).

- facet_by:

  optional character string naming a column to facet by (e.g.
  "Condition", "ContaminantGroup").

## Value

ggplot object.

## Examples

``` r
if (FALSE) { # \dontrun{
plot_heatmap_conta(conta)
plot_heatmap_conta(conta, x = Sample, facet_by = "ContaminantGroup")
} # }
```
