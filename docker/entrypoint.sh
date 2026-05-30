#!/bin/bash
set -e

usage() {
  cat <<EOF
HowDirty — Generate contamination reports from Skyline CSV exports

Usage:
  docker run --rm -v /path/to/data:/data ghcr.io/davidgz1/howdirty [OPTIONS]

Options:
  --dataset NAME              Experiment name; used as output filename prefix  [required*]
  --peak-areas FILE           Path to Skyline PeakAreas_Contaminants CSV       [required*]
  --annotation FILE           Path to sample annotation CSV                    [required*]
  --ref-thresholds FILE       Path to reference HowDirty Excel output (optional)
  --output-dir DIR            Output directory (default: /data/results)
  --interactive-plots         Enable interactive plotly plots (default: static)
  --user-names TEXT           Analyst name(s) embedded in report header
  --notes TEXT                Notes embedded in report header
  --keep-missing-contaminants Keep contaminants not detected in any sample (default: removed)
  --top-n INT                 Number of top contaminant groups in plots (default: 10)
  --multiply-dilution-factor  Multiply abundance by DilutionFactor column (default: off)
  --get-template              Generate a blank annotation CSV from peak areas file

  *required unless --get-template is used

Environment variable equivalents:
  DATASET, FILE_PEAK_AREAS, FILE_ANNOTATION, FILE_REF_THRESHOLDS,
  OUTPUT_DIR, PLOTS_INTERACTIVE, USER_NAMES, NOTES,
  REMOVE_MISSING_CONTAMINANTS, N_TOP_CONTAMINANT_GROUPS, MULTIPLY_DILUTION_FACTOR,
  GET_TEMPLATE

Examples:
  # Step 1 — create annotation template
  docker run --rm -v \$(pwd):/data ghcr.io/davidgz1/howdirty \\
    --get-template --peak-areas /data/PeakAreas.csv

  # Step 2 — generate report
  docker run --rm -v \$(pwd):/data ghcr.io/davidgz1/howdirty \\
    --dataset MyExp --peak-areas /data/PeakAreas.csv --annotation /data/annotation.csv
EOF
}

# ---- defaults (env vars provide fallbacks) ----
DATASET="${DATASET:-}"
FILE_PEAK_AREAS="${FILE_PEAK_AREAS:-}"
FILE_ANNOTATION="${FILE_ANNOTATION:-}"
FILE_REF_THRESHOLDS="${FILE_REF_THRESHOLDS:-FALSE}"
OUTPUT_DIR="${OUTPUT_DIR:-/data/results}"
PLOTS_INTERACTIVE="${PLOTS_INTERACTIVE:-FALSE}"
USER_NAMES="${USER_NAMES:-}"
NOTES="${NOTES:-}"
REMOVE_MISSING_CONTAMINANTS="${REMOVE_MISSING_CONTAMINANTS:-TRUE}"
N_TOP_CONTAMINANT_GROUPS="${N_TOP_CONTAMINANT_GROUPS:-10}"
MULTIPLY_DILUTION_FACTOR="${MULTIPLY_DILUTION_FACTOR:-FALSE}"
GET_TEMPLATE="${GET_TEMPLATE:-FALSE}"

# ---- parse CLI flags ----
while [[ $# -gt 0 ]]; do
  case "$1" in
    --dataset)           DATASET="$2";            shift 2 ;;
    --peak-areas)        FILE_PEAK_AREAS="$2";     shift 2 ;;
    --annotation)        FILE_ANNOTATION="$2";     shift 2 ;;
    --ref-thresholds)    FILE_REF_THRESHOLDS="$2"; shift 2 ;;
    --output-dir)        OUTPUT_DIR="$2";          shift 2 ;;
    --interactive-plots)         PLOTS_INTERACTIVE=TRUE;            shift   ;;
    --user-names)                USER_NAMES="$2";                   shift 2 ;;
    --notes)                     NOTES="$2";                        shift 2 ;;
    --keep-missing-contaminants) REMOVE_MISSING_CONTAMINANTS=FALSE; shift   ;;
    --top-n)                     N_TOP_CONTAMINANT_GROUPS="$2";     shift 2 ;;
    --multiply-dilution-factor)  MULTIPLY_DILUTION_FACTOR=TRUE;     shift   ;;
    --get-template)              GET_TEMPLATE=TRUE;                 shift   ;;
    --help|-h)           usage; exit 0 ;;
    *) echo "Unknown argument: $1"; echo; usage; exit 1 ;;
  esac
done

# ---- validate ----
if [[ "$GET_TEMPLATE" == "TRUE" ]]; then
  if [[ -z "$FILE_PEAK_AREAS" ]]; then
    echo "Error: --peak-areas is required with --get-template"
    exit 1
  fi
else
  if [[ -z "$DATASET" || -z "$FILE_PEAK_AREAS" || -z "$FILE_ANNOTATION" ]]; then
    echo "Error: --dataset, --peak-areas, and --annotation are required"
    echo
    usage
    exit 1
  fi
fi

# ---- export for run_report.R ----
export DATASET FILE_PEAK_AREAS FILE_ANNOTATION FILE_REF_THRESHOLDS \
       OUTPUT_DIR PLOTS_INTERACTIVE USER_NAMES NOTES \
       REMOVE_MISSING_CONTAMINANTS N_TOP_CONTAMINANT_GROUPS MULTIPLY_DILUTION_FACTOR \
       GET_TEMPLATE

exec Rscript /usr/local/lib/howdirty_run.R
