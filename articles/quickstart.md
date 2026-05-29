# Getting started with HowDirty

HowDirty quantifies molecular contamination in LC-MS experiments. It
reads peak area exports from [Skyline](https://skyline.ms) and produces
a self-contained HTML report with risk-scored tables and plots for each
contaminant, sample, and experimental condition.

## Installation

``` r

# install.packages("remotes")
remotes::install_github("DavidGZ1/HowDirty")
```

``` r

library(HowDirty)
```

## Input files

Two files are required:

**1. Skyline peak-area export** — a CSV produced by Skyline’s *File →
Export → Report* dialog using a contaminant report definition. The file
must contain columns for protein (contaminant name), peptide, replicate
name, retention time, total MS1 area, and total ion current area.

**2. Sample annotation** — a CSV mapping Skyline replicate names to
experimental metadata. Generate a blank template from your data with:

``` r

get_annotation_template(file_report_skyline = "PeakAreas_Contaminants.csv",
                        save = TRUE)
```

The resulting file has four columns: `ReplicateNameSkyline` (required),
`Sample`, `Condition`, and `DilutionFactor` (all optional; defaults are
applied if omitted). Add any extra metadata columns you need
(e.g. Batch, Instrument).

## Step-by-step workflow

### 1. Read the Skyline export

``` r

conta_raw <- read_conta_results("PeakAreas_Contaminants.csv")
```

Returns a data frame with one row per contaminant × replicate. The key
derived column is `Abundance = TotalAreaMS1 / TotalIonCurrentArea`
(TIC-normalised).

### 2. Read the sample annotation

``` r

samples_annot <- read_samples_annotation("samples_annotation.csv")
```

### 3. Merge results and annotations

``` r

conta <- annotate_conta_samples(conta_raw, samples_annot)
```

Optional: set `multiply_dilution_factor = TRUE` if your annotation
includes meaningful dilution factors that should scale the abundances.

### 4. Set contamination thresholds

**Without a reference dataset** — use uniform thresholds derived from
built-in quantile breakpoints:

``` r

thresholds <- get_simple_thresholds_contaminant(conta)
```

**With a reference dataset** — read quantile thresholds from a previous
HowDirty Excel output (the sheet `conta_summ_contaminant`):

``` r

thresholds <- read_conta_thresholds("reference_report.xlsx")
```

Reference thresholds give more meaningful risk scores by comparing your
samples against a historical baseline.

### 5. Assign risk levels

``` r

conta <- annotate_conta_thresholds(conta, thresholds, Abundance)
```

Each row is assigned `RiskLevel` (0–6) and a human-readable `Risk`
label. See `RISK_LABELS` for the full mapping.

### 6. Summarise

[`summarize_conta()`](https://davidgz1.github.io/HowDirty/reference/summarize_conta.md)
accepts any grouping variables:

``` r

# Per contaminant group
summ_cg <- summarize_conta(conta, ContaminantGroup)

# Per condition and contaminant group
summ_cond <- summarize_conta(conta, Condition, ContaminantGroup)

# Per sample
summ_sample <- summarize_conta(conta, Condition, Sample, ReplicateName)
```

Each summary includes
`Abundance_{min, quantile25, median, quantile75, quantile90, max, total}`.

### 7. Plot

``` r

plot_heatmap_conta(conta)

plot_sample_risk_total(summ_sample)
```

All plot functions return `ggplot2` objects and accept a `rotate_x`
argument to tilt x-axis labels.

## One-liner: generate a full HTML report

For a complete report in a single call:

``` r

generate_howdirty_report(
  dataset            = "Experiment2021",
  file_peak_areas    = "PeakAreas_Contaminants.csv",
  file_annotation    = "samples_annotation.csv",
  file_ref_thresholds = FALSE,   # or path to reference .xlsx
  output_dir         = "results"
)
```

This renders the bundled RMarkdown template and writes a self-contained
HTML file plus an Excel summary workbook. Use
[`get_report_template()`](https://davidgz1.github.io/HowDirty/reference/get_report_template.md)
to copy the template to your working directory for customisation.

## Batch processing

Run the same report across multiple datasets at once:

``` r

run_howdirty_batch(
  datasets = list(
    Exp1 = list(file_peak_areas = "exp1/PeakAreas.csv",
                file_annotation = "exp1/annotation.csv"),
    Exp2 = list(file_peak_areas = "exp2/PeakAreas.csv",
                file_annotation = "exp2/annotation.csv")
  ),
  file_ref_thresholds = "reference_report.xlsx",
  output_dir          = "results"
)
```

## Running with Docker

If you don’t have R installed, the official Docker image lets you run
HowDirty with no local setup. All input files must be in a single
directory that is mounted as `/data` inside the container.

### Step 1 — generate an annotation template

``` bash
docker run --rm -v $(pwd):/data ghcr.io/davidgz1/howdirty \
  --get-template \
  --peak-areas /data/PeakAreas_Contaminants.csv
```

This writes `samples_annotation_template.csv` to your working directory.
Fill in the `Sample`, `Condition`, and `DilutionFactor` columns before
the next step.

### Step 2 — generate the report

``` bash
docker run --rm -v $(pwd):/data ghcr.io/davidgz1/howdirty \
  --dataset MyExperiment \
  --peak-areas /data/PeakAreas_Contaminants.csv \
  --annotation /data/samples_annotation.csv
```

Output is written to `./results/` by default:

- `MyExperiment_HowDirtyReport.html` — self-contained interactive report
- `MyExperiment_report_contaminants_<timestamp>.xlsx` — summary workbook

### Using a reference threshold file

``` bash
docker run --rm -v $(pwd):/data ghcr.io/davidgz1/howdirty \
  --dataset MyExperiment \
  --peak-areas /data/PeakAreas_Contaminants.csv \
  --annotation /data/samples_annotation.csv \
  --ref-thresholds /data/reference_report.xlsx
```

### All options

| Flag | Default | Description |
|----|----|----|
| `--dataset` | — | Experiment name; used as output filename prefix |
| `--peak-areas` | — | Path to Skyline PeakAreas_Contaminants CSV |
| `--annotation` | — | Path to sample annotation CSV |
| `--ref-thresholds` | none | Path to reference HowDirty Excel output |
| `--output-dir` | `/data/results` | Output directory |
| `--interactive-plots` | off | Enable interactive plotly plots |
| `--user-names` | — | Analyst name(s) embedded in report header |
| `--notes` | — | Notes embedded in report header |
| `--get-template` | — | Generate blank annotation CSV only |

Every flag has an equivalent environment variable (`DATASET`,
`FILE_PEAK_AREAS`, `FILE_ANNOTATION`, etc.) — useful for scripting or CI
pipelines:

``` bash
docker run --rm \
  -v $(pwd):/data \
  -e DATASET=MyExperiment \
  -e FILE_PEAK_AREAS=/data/PeakAreas_Contaminants.csv \
  -e FILE_ANNOTATION=/data/samples_annotation.csv \
  ghcr.io/davidgz1/howdirty
```

## Further reading

- Full function reference:
  [`help(package = "HowDirty")`](https://davidgz1.github.io/HowDirty/reference)
- Parameterised RMarkdown template:
  [`get_report_template()`](https://davidgz1.github.io/HowDirty/reference/get_report_template.md)
- Multi-dataset comparison:
  [`?compare_howdirty`](https://davidgz1.github.io/HowDirty/reference/compare_howdirty.md)
