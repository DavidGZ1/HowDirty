# Summarize the contaminant sample set

Summarizes the contaminant sample set to get a summary per sample, per
condition, and for all samples

## Usage

``` r
summarize_conta_sampleset(df_conta, df_threshold_sample)
```

## Arguments

- df_conta:

  dataframe containing the values to summarize.

- df_threshold_sample:

  dataframe containing sample-level thresholds (from
  read_conta_sample_thresholds or get_simple_thresholds_sample).

## Value

dataframe containing the summaries.

## Examples

``` r
if (FALSE) { # \dontrun{
summarize_conta_sampleset(df_conta, ref_conta_tshd_sample)
} # }
```
