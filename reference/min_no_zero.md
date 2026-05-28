# Minima without zero

Returns the minima of the input values while ignoring zero values.

## Usage

``` r
min_no_zero(x, na.rm = TRUE)
```

## Arguments

- x:

  numeric vector.

- na.rm:

  logical; if TRUE (default) NAs are removed before computing the
  minimum.

## Value

numeric vector.

## Examples

``` r
if (FALSE) { # \dontrun{
min_no_zero(c(0,1,2,3,10))
} # }
```
