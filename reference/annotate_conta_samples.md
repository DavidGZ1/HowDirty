# Annotate contaminated samples

Annotates the contamination results using the samples annotation,
multiplies by dilution factor, and assigns risc level

## Usage

``` r
annotate_conta_samples(
  df_conta,
  df_samples_annot,
  remove_missing = FALSE,
  multiply_dilution_factor = FALSE
)
```

## Arguments

- df_conta:

  dataframe containing the contaminated samples.

- df_samples_annot:

  dataframe with annotated samples.

- remove_missing:

  flag if missing values should be removed.

- multiply_dilution_factor:

  flag if multiplication by dilution factor happens.

## Value

merged dataframe containing contaminated and annotated samples.

## Examples

``` r
if (FALSE) { # \dontrun{
annotate_conta_samples(df_conta, df_samples_annot, remove_missing = FALSE, multiply_dilution_factor = FALSE)
} # }
```
