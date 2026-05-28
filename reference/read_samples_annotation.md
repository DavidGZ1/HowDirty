# Read Sample Annotation

Reads the annotation CSV and handles renaming and missing value
defaults. Required columns: ReplicateNameSkyline. Optional: Sample,
Condition, DilutionFactor. Any additional columns (e.g. Batch,
Instrument, Operator) are preserved as-is and carried through the
pipeline by
[`annotate_conta_samples()`](https://davidgz1.github.io/HowDirty/reference/annotate_conta_samples.md).

## Usage

``` r
read_samples_annotation(file_samples_annotation)
```

## Arguments

- file_samples_annotation:

  Input file containing sample annotation.

## Value

dataframe with annotation columns; extra metadata columns are retained.

## Examples

``` r
if (FALSE) { # \dontrun{
read_samples_annotation("samples_annotation.csv")
} # }
```
