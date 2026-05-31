# HowDirty

## About

HowDirty is an R package that assesses the level of contamination of
LC-MS results. The presence of contaminants (e.g., PEG), and detergents
(e.g., CHAPS, SDS) in samples analyzed by LC-MS can be severely
detrimental to identifying peptides/proteins or other molecules. Skyline
is used to extra MS1 features of many known contaminant masses from raw
files (e.g., .raw and .d). The results are exported to a .csv file, then
processed in R using HowDirty to generate an HTML interactive report
that evaluates sample contamination risks. For more details, please see
our [publication](https://doi.org/10.1002/pmic.202300134) and the
tutorial (below).

## References - please cite:

HowDirty R package: D. Gomez‐Zepeda, T. Michna, T. Ziesmann, U. Distler,
S. Tenzer, HowDirty: An R package to evaluate molecular contaminants in
LC‐MS experiments, Proteomics. (2023) 1–6.
<https://doi.org/10.1002/pmic.202300134>.

Skyline: B. MacLean, D.M. Tomazela, N. Shulman, M. Chambers, G.L.
Finney, B. Frewen, R. Kern, D.L. Tabb, D.C. Liebler, M.J. MacCoss,
Skyline: an open source document editor for creating and analyzing
targeted proteomics experiments, Bioinformatics. 26 (2010) 966–968.
\[<https://doi.org/10.1093/bioinformatics/btq054>\].

Molecular contaminant transition list: M.J. Rardin, Rapid Assessment of
Contaminants and Interferences in Mass Spectrometry Data Using Skyline,
J. Am. Soc. Mass Spectrom. 29 (2018) 1327–1330.
<https://doi.org/10.1007/s13361-018-1940-z>.

## Tutorial

Detailed instructions can be found in the
[tutorial](https://github.com/DavidGZ1/HowDirty/blob/main/tutorial/HowDirty_tutorial.pdf).

## Requirements

- Raw LC-MS results to be evaluated
- Skyline version \> 4 \[1,2\]. To install it, you can register online
  and download the latest version here:
  <https://skyline.ms/project/home/software/Skyline/begin.view>
- Skyline HowDirty
  [template](https://github.com/DavidGZ1/HowDirty/tree/main/tutorial),
  including the Skyline molecular contaminant transition list \[3\] and
  reports configuration
- Alternatively, you can set up Skyline yourself (further instructions
  in the [Skyline
  tutorials](https://skyline.ms/wiki/home/software/Skyline/page.view?name=tutorials))
  - Download the molecular contaminant transition list \[3\] from
    [Panorama](https://panoramaweb.org/project/Panorama%20Public/2018/Amgen%20-%20Molecular%20Contaminants/begin.view?)
    and load it into Skyline:File / Import / Transition List…
  - Create the PeakAreas_Contaminants report: containing the columns:
  - Settings / Document Settings / Report / Add, then add a name and
    select the columns: “Protein”, “Peptide”, “Replicate Name”, “Peptide
    Retention Time”, “Total Area MS1”, “Total Ion Current Area”
  - Enable the report form by ticking the box next to its name, then
    click OK
- Run HowDirty through any of these options:
  - **Windows launcher** *(easiest — no coding or terminal required)*:
    download
    [`launch_howdirty.bat`](https://github.com/DavidGZ1/HowDirty/releases/latest)
    and double-click it. Docker Desktop is started automatically if
    needed and the app opens in your browser. Windows only.
  - **Browser app** *(easy — no coding required, any OS)*: start the
    Shiny web interface with one Docker command
    (`docker run --rm -p 3838:3838 ghcr.io/davidgz1/howdirty:app`) and
    open `http://localhost:3838`. Upload your files and download the
    results without writing any code.
  - **Docker CLI** *(intermediate — basic terminal knowledge required)*:
    run the full report pipeline from the command line using
    `docker run`. Useful for batch processing or scripting. Requires
    Docker Desktop.
  - **R package** *(advanced — R knowledge required)*: install HowDirty
    directly in R for maximum flexibility, programmatic batch
    processing, and integration with existing R workflows.

## Installation

``` r

install.packages("devtools")
library(devtools)
install_github("kassambara/ggpubr")
install_github("DavidGZ1/HowDirty", force = TRUE)
```

## Usage

See tutorial for detailed instructions on how to extract MS1 features
from Skyline and on pipeline usage.

``` r

#  Set the working directory to the desired folder (e.g., where the PeakAreas_Contaminants.csv is stored)
setwd("C:/Users/Name/ExampleHowDirty")

# Load HowDirty package
library(HowDirty)

# Create sample annotation or experiment design file
get_annotation_template(file_report_skyline  = "PeakAreas_Contaminants.csv")

# Create HowDirty template with the name "example.Rmd"
HowDirty::get_report_template(file = "example")

# Fill the parameters in the header and “knit” (compile) the report. There are two options for this (see screenshots below).
# Using RStudio:
#    a) Click on "Knit with parameters"
#    b) Fill out the parameters values manually, then click "Knit"
#       The parameters are in the header of the .Rmd document after "params:"
#       The values are entered after "value: "
#       Do not modify the text after "label: " or "input: "
```

Knit button in RStudio:

![Button options in RStudio for knitting a markdown file. Shots the Knit
button with a dropdown menu, that includes ‘Knit to HTML’, ‘Knit to
PDF’, ‘Knit to Word’, ‘Knit with
Parameters’.](https://github.com/DavidGZ1/HowDirty/assets/134387857/be25535d-6583-4b75-8f64-09a407d1d5cf)

Button options in RStudio for knitting a markdown file. Shots the Knit
button with a dropdown menu, that includes ‘Knit to HTML’, ‘Knit to
PDF’, ‘Knit to Word’, ‘Knit with Parameters’.

1.  

![Screenshot of the window popping up when using ‘Knit with Parameters’.
It shows several text fields with
descriptions.](https://github.com/DavidGZ1/HowDirty/assets/134387857/407eba9a-fe0a-47d4-99e6-21874aa47943)

Screenshot of the window popping up when using ‘Knit with Parameters’.
It shows several text fields with descriptions.

2.  

![Screenshot of the parameters section of the .Rmd file, showing where
the file names for the PeakAreasContaminantsFile, the AnnotationFile,
and (if applicable) the RefThresholdsFile need to be added with
examples.](https://github.com/DavidGZ1/HowDirty/assets/134387857/dff65428-d7d0-4e12-9039-49941954cafd)

Screenshot of the parameters section of the .Rmd file, showing where the
file names for the PeakAreasContaminantsFile, the AnnotationFile, and
(if applicable) the RefThresholdsFile need to be added with examples.

## Web app (no coding required)

The easiest way to use HowDirty — upload your files in a browser and
download the results. No R, no command line, no volume mounts.

``` bash
docker run --rm -p 3838:3838 ghcr.io/davidgz1/howdirty:app
```

Then open **<http://localhost:3838>** in your browser.

The app walks you through uploading your peak areas file, generating and
filling the annotation template, and downloading the HTML report and
Excel workbook. All parameters available in the Docker CLI (reference
thresholds, top-n groups, dilution factor, interactive plots, etc.) can
also be set in the interface under **Advanced options**.

------------------------------------------------------------------------

## Running with Docker (CLI)

If you don’t have R installed, you can run HowDirty using Docker — no R
setup required.

### Step 0 — Install Docker and add it to PATH

1.  Download and install **Docker Desktop** from
    <https://www.docker.com/products/docker-desktop>
2.  Launch Docker Desktop and wait until it shows “Docker is running”
3.  Add the Docker CLI to your PATH so you can run `docker` commands
    from any terminal:

**Windows (PowerShell) — current session only:**

``` powershell
$env:PATH += ";C:\Program Files\Docker\Docker\resources\bin"
```

**Windows (PowerShell) — permanent (all future sessions):**

``` powershell
[System.Environment]::SetEnvironmentVariable(
    "PATH",
    $env:PATH + ";C:\Program Files\Docker\Docker\resources\bin",
    "User"
)
```

Then restart PowerShell for the change to take effect.

**macOS / Linux:** Docker Desktop adds the CLI to PATH automatically
during installation. If `docker` is not found, restart your terminal or
log out and back in.

Verify the installation:

``` bash
docker --version
```

### Step 1 — Pull the image

``` bash
docker pull ghcr.io/davidgz1/howdirty:latest
```

### Step 2 — Generate an annotation template

Navigate to your data folder first, then run:

``` bash
# Linux / macOS
docker run --rm -v $(pwd):/data ghcr.io/davidgz1/howdirty \
  --get-template --peak-areas /data/PeakAreas_Contaminants.csv
```

``` powershell
# Windows (PowerShell)
docker run --rm -v "${PWD}:/data" ghcr.io/davidgz1/howdirty `
  --get-template --peak-areas /data/PeakAreas_Contaminants.csv
```

This writes `samples_annotation_template.csv` to your current directory.
Fill it in before the next step.

### Step 3 — Generate the report

``` bash
# Linux / macOS
docker run --rm -v $(pwd):/data ghcr.io/davidgz1/howdirty \
  --dataset MyExperiment \
  --peak-areas /data/PeakAreas_Contaminants.csv \
  --annotation /data/samples_annotation.csv
```

``` powershell
# Windows (PowerShell)
docker run --rm -v "${PWD}:/data" ghcr.io/davidgz1/howdirty `
  --dataset MyExperiment `
  --peak-areas /data/PeakAreas_Contaminants.csv `
  --annotation /data/samples_annotation.csv
```

This writes `MyExperiment_HowDirtyReport.html` and `.xlsx` to a
`results/` subfolder in your data directory.

### Optional flags

| Flag | Description | Default |
|----|----|----|
| `--ref-thresholds FILE` | Reference HowDirty Excel output for threshold comparison | none |
| `--output-dir DIR` | Output directory inside the container | `/data/results` |
| `--interactive-plots` | Enable interactive plotly plots | static |
| `--user-names TEXT` | Analyst name(s) shown in the report header | — |
| `--notes TEXT` | Notes shown in the report header | — |
| `--keep-missing-contaminants` | Keep contaminants not detected in any sample | removed |
| `--top-n INT` | Number of top contaminant groups shown in plots | 10 |
| `--multiply-dilution-factor` | Multiply abundance by the DilutionFactor column | off |

Run `docker run --rm ghcr.io/davidgz1/howdirty --help` for the full
reference.

## License

[GPL-3.0](https://github.com/DavidGZ1/HowDirty/blob/main/LICENSE)
