# Get the HowDirtyReport template

This is a wrapper using rmarkdown::draft to get the HowDirtyReport
template copied in the working directory

## Usage

``` r
get_report_template(
  file = "HowDirtyReportTemplate",
  edit = FALSE,
  create_dir = FALSE
)
```

## Arguments

- file:

  name of the report file to be copied.

- edit:

  flag.

- create_dir:

  flag if directory should be created.

## Value

None

## Examples

``` r
if (FALSE) { # \dontrun{
get_report_template(file = "HowDirtyReportTemplate", edit = TRUE, create_dir = FALSE)
} # }
```
