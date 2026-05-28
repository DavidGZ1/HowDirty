# Check if all the required columns are present in the Skyline export (df_conta)

read.csv using sep = "," or sep = ";" (read.csv) in function of the
header

## Usage

``` r
read.csv_auto_sep(file, ...)
```

## Arguments

- file:

  the name of the file which the data are to be read from. Each row of
  the table appears as one line of the file. If it does not contain an
  absolute path, the file name is relative to the current working
  directory, getwd(). Tilde-expansion is performed where supported. This
  can be a compressed file (see file).

- ...:

  Further arguments to be passed to read.csv or read.csv2.

## Value

None

## Examples

``` r
if (FALSE) { # \dontrun{
read.csv_auto_sep("PeakAreas_Contaminants.csv")
} # }
```
