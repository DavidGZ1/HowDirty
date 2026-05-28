#' Compare HowDirty results across multiple datasets
#'
#' Reads the \code{conta_summ_sample} sheet from each HowDirty Excel output and
#' combines them into a single dataframe with a \code{Dataset} column, ready for
#' cross-dataset plotting or summary.
#'
#' @param files named character vector (or named list) mapping dataset labels to
#'   HowDirty Excel file paths. Names become the \code{Dataset} column values.
#'
#' @return dataframe combining \code{conta_summ_sample} from all files, with an
#'   added \code{Dataset} factor column.
#'
#' @examples
#' \dontrun{
#' combined <- compare_howdirty(c(
#'   Instrument_A = "results/A_report.xlsx",
#'   Instrument_B = "results/B_report.xlsx"
#' ))
#' plot_comparison_conta(combined)
#' }
#'
#' @export
compare_howdirty <- function(files){
  if(is.null(names(files)) || any(names(files) == "")){
    stop("All entries in 'files' must be named (names become Dataset labels).")
  }
  missing_files <- files[!file.exists(unlist(files))]
  if(length(missing_files) > 0){
    stop("File(s) not found: ", paste(names(missing_files), collapse = ", "))
  }

  dfs <- lapply(names(files), function(nm){
    df <- readxl::read_xlsx(unlist(files[nm]), sheet = "conta_summ_sample")
    df$Dataset <- nm
    df
  })

  output <- do.call(rbind, dfs)
  output$Dataset <- factor(output$Dataset, levels = names(files))
  output
}


#' Plot contamination comparison across datasets
#'
#' Boxplot of total contamination abundance per dataset. Useful for comparing
#' contamination levels across instruments, batches, or time periods.
#'
#' @param df_combined combined dataframe from \code{\link{compare_howdirty}}.
#' @param scale "linear" or "log10" for the y-axis (default: "linear").
#' @param compare_means logical; add Wilcoxon pairwise comparison (default: FALSE).
#'
#' @return ggplot object.
#'
#' @examples
#' \dontrun{
#' plot_comparison_conta(combined)
#' plot_comparison_conta(combined, scale = "log10", compare_means = TRUE)
#' }
#'
#' @export
plot_comparison_conta <- function(df_combined, scale = "linear", compare_means = FALSE){
  if(!"Dataset" %in% names(df_combined)){
    stop("df_combined must contain a 'Dataset' column. Use compare_howdirty() to create it.")
  }

  output <-
    ggplot(df_combined, aes(x = Dataset, y = Abundance_total)) +
    geom_boxplot(alpha = 0.4, width = 0.5, linewidth = 0.3,
                 outlier.shape = NA) +
    geom_jitter(aes(color = RiskLevel), width = 0.15, size = 1.5, alpha = 0.7) +
    scale_color_risk() +
    ylab("Total Abundance") +
    xlab(NULL) +
    theme_hd() +
    rotate_x_text(angle = 45)

  if(compare_means){
    output <- output +
      ggpubr::stat_compare_means(method = "wilcox.test", size = 2.5,
                                  comparisons = utils::combn(levels(df_combined$Dataset),
                                                             2, simplify = FALSE))
  }
  if(scale == "log10"){
    output <- output + scale_y_log10() + ylab("log10(Total Abundance)")
  }
  return(output)
}
