# Package index

## Package

- [`HowDirty-package`](https://davidgz1.github.io/HowDirty/reference/HowDirty.md)
  [`HowDirty`](https://davidgz1.github.io/HowDirty/reference/HowDirty.md)
  : Evaluate How Dirty are LC-MS samples

## Data import

- [`read_conta_results()`](https://davidgz1.github.io/HowDirty/reference/read_conta_results.md)
  : Read Conta Results
- [`read_samples_annotation()`](https://davidgz1.github.io/HowDirty/reference/read_samples_annotation.md)
  : Read Sample Annotation
- [`read_conta_thresholds()`](https://davidgz1.github.io/HowDirty/reference/read_conta_thresholds.md)
  : Read contaminant thresholds
- [`read_conta_sample_thresholds()`](https://davidgz1.github.io/HowDirty/reference/read_conta_sample_thresholds.md)
  : Read thresholds for sample level
- [`read.csv_auto_sep()`](https://davidgz1.github.io/HowDirty/reference/read.csv_auto_sep.md)
  : Check if all the required columns are present in the Skyline export
  (df_conta)

## Annotation

- [`annotate_conta_samples()`](https://davidgz1.github.io/HowDirty/reference/annotate_conta_samples.md)
  : Annotate contaminated samples
- [`annotate_conta_thresholds()`](https://davidgz1.github.io/HowDirty/reference/annotate_conta_thresholds.md)
  : Annotate the threshold for contaminants
- [`annotate_conta_thresholds_samples()`](https://davidgz1.github.io/HowDirty/reference/annotate_conta_thresholds_samples.md)
  : Annotate contaminant
- [`annotate_contagroup_thresholds()`](https://davidgz1.github.io/HowDirty/reference/annotate_contagroup_thresholds.md)
  : Annotate thresholds of contaminant groups

## Summarisation

- [`summarize_conta()`](https://davidgz1.github.io/HowDirty/reference/summarize_conta.md)
  : Summarize Contaminant Groups
- [`summarize_conta_sampleset()`](https://davidgz1.github.io/HowDirty/reference/summarize_conta_sampleset.md)
  : Summarize the contaminant sample set
- [`arrange_contagroup_levels()`](https://davidgz1.github.io/HowDirty/reference/arrange_contagroup_levels.md)
  : Arrange contaminant group levels

## Risk thresholds

- [`get_simple_thresholds_contaminant()`](https://davidgz1.github.io/HowDirty/reference/get_simple_thresholds_contaminant.md)
  : Set arbitrary contaminant threshold
- [`get_simple_thresholds_sample()`](https://davidgz1.github.io/HowDirty/reference/get_simple_thresholds_sample.md)
  : Get thresholds for sample file
- [`RISK_LABELS`](https://davidgz1.github.io/HowDirty/reference/RISK_LABELS.md)
  : Risk level labels

## Plotting

- [`plot_abundance()`](https://davidgz1.github.io/HowDirty/reference/plot_abundance.md)
  : Plot abundance
- [`plot_comparison_conta()`](https://davidgz1.github.io/HowDirty/reference/plot_comparison_conta.md)
  : Plot contamination comparison across datasets
- [`plot_condition_risk_contaminant()`](https://davidgz1.github.io/HowDirty/reference/plot_condition_risk_contaminant.md)
  : Plot contaminant vs condition
- [`plot_condition_risk_total_boxplot()`](https://davidgz1.github.io/HowDirty/reference/plot_condition_risk_total_boxplot.md)
  : Plot abundance
- [`plot_contaminantgroup_risk()`](https://davidgz1.github.io/HowDirty/reference/plot_contaminantgroup_risk.md)
  : Plot contaminant group risk
- [`plot_heatmap_conta()`](https://davidgz1.github.io/HowDirty/reference/plot_heatmap_conta.md)
  : Heatmap of contamination risk
- [`plot_pseudochromatogram()`](https://davidgz1.github.io/HowDirty/reference/plot_pseudochromatogram.md)
  : Plot pseudochromatogram
- [`plot_risk_summ_sampleset()`](https://davidgz1.github.io/HowDirty/reference/plot_risk_summ_sampleset.md)
  : Plot samples associated to risk level
- [`plot_sample_risk_contaminant()`](https://davidgz1.github.io/HowDirty/reference/plot_sample_risk_contaminant.md)
  : Plot contaminant abundance
- [`plot_sample_risk_total()`](https://davidgz1.github.io/HowDirty/reference/plot_sample_risk_total.md)
  : Plot total abundance
- [`plot_trend_conta()`](https://davidgz1.github.io/HowDirty/reference/plot_trend_conta.md)
  : Longitudinal trend plot of total contamination
- [`scale_color_risk()`](https://davidgz1.github.io/HowDirty/reference/scale_color_risk.md)
  : Colour scale risk
- [`scale_fill_risk()`](https://davidgz1.github.io/HowDirty/reference/scale_fill_risk.md)
  : Fill scale risk
- [`scale_fill_risk_level()`](https://davidgz1.github.io/HowDirty/reference/scale_fill_risk_level.md)
  : Scale fill risk level
- [`theme_hd()`](https://davidgz1.github.io/HowDirty/reference/theme_hd.md)
  : HowDirty theme
- [`layout_ggplotly_label_margin()`](https://davidgz1.github.io/HowDirty/reference/layout_ggplotly_label_margin.md)
  : Label margin

## Report generation

- [`generate_howdirty_report()`](https://davidgz1.github.io/HowDirty/reference/generate_howdirty_report.md)
  : Generate a HowDirty report programmatically
- [`run_howdirty_batch()`](https://davidgz1.github.io/HowDirty/reference/run_howdirty_batch.md)
  : Batch-generate HowDirty reports
- [`run_howdirty_app()`](https://davidgz1.github.io/HowDirty/reference/run_howdirty_app.md)
  : Launch the HowDirty Shiny web app
- [`get_report_template()`](https://davidgz1.github.io/HowDirty/reference/get_report_template.md)
  : Get the HowDirtyReport template
- [`compare_howdirty()`](https://davidgz1.github.io/HowDirty/reference/compare_howdirty.md)
  : Compare HowDirty results across multiple datasets

## Utilities

- [`get_annotation_template()`](https://davidgz1.github.io/HowDirty/reference/get_annotation_template.md)
  : Create a dataframe with the columns required for the annotation file
  This function creates a dataframe with the columns required for the
  annotation file. If a file_report_skyline is provided, the
  ReplicateNameSkyline and Sample columns are populated with
  unique(input\$Replicate.Name).
- [`check_conta_columns()`](https://davidgz1.github.io/HowDirty/reference/check_conta_columns.md)
  : Check if all the required columns are present in the Skyline export
  (df_conta)
- [`check_samples_in_results()`](https://davidgz1.github.io/HowDirty/reference/check_samples_in_results.md)
  : Check if samples are included in both results and annotation
- [`count_factor_columns()`](https://davidgz1.github.io/HowDirty/reference/count_factor_columns.md)
  : Short description
- [`min_no_zero()`](https://davidgz1.github.io/HowDirty/reference/min_no_zero.md)
  : Minima without zero
- [`get_h()`](https://davidgz1.github.io/HowDirty/reference/get_h.md) :
  Short description
- [`get_w()`](https://davidgz1.github.io/HowDirty/reference/get_w.md) :
  Short description
- [`get_maintainer()`](https://davidgz1.github.io/HowDirty/reference/get_maintainer.md)
  : Get string information for Maintainer
- [`colorize_text()`](https://davidgz1.github.io/HowDirty/reference/colorize_text.md)
  : Colorize html output
