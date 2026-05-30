# Launch the HowDirty Shiny web app

Opens a browser-based GUI for generating contamination reports without
writing any R code. Requires the shiny and bslib packages.

## Usage

``` r
run_howdirty_app(port = 3838, launch.browser = interactive())
```

## Arguments

- port:

  Port to listen on (default 3838).

- launch.browser:

  Open a browser window automatically (default `TRUE` in interactive
  sessions).

## Value

Does not return; starts the Shiny server.

## Examples

``` r
if (FALSE) { # \dontrun{
run_howdirty_app()
} # }
```
