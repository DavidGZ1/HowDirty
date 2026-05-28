make_summ_sample <- function(n = 6) {
  data.frame(
    Condition      = rep("A", n),
    Sample         = paste0("S", seq_len(n)),
    ReplicateName  = paste0("Run", seq_len(n)),
    Abundance_total = seq(0.1, 0.6, length.out = n),
    RiskLevel      = factor(c(1, 1, 2, 2, 3, 3), levels = 0:6),
    Risk           = factor(c("1) Very Low", "1) Very Low", "2) Low", "2) Low",
                              "3) Medium", "3) Medium")),
    stringsAsFactors = FALSE
  )
}

# --- plot_trend_conta ---

test_that("plot_trend_conta returns a ggplot", {
  p <- plot_trend_conta(make_summ_sample())
  expect_s3_class(p, "ggplot")
})

test_that("plot_trend_conta accepts custom x column", {
  expect_no_error(plot_trend_conta(make_summ_sample(), x = "Sample"))
})

test_that("plot_trend_conta errors on unknown x column", {
  expect_error(plot_trend_conta(make_summ_sample(), x = "nonexistent"),
               regexp = "not found")
})

test_that("plot_trend_conta with add_smooth = FALSE has no smooth layer", {
  p <- plot_trend_conta(make_summ_sample(), add_smooth = FALSE)
  layer_classes <- sapply(p$layers, function(l) class(l$geom)[1])
  expect_false("GeomSmooth" %in% layer_classes)
})

test_that("plot_trend_conta facet_by adds facet layer", {
  df <- make_summ_sample()
  df$Condition <- rep(c("A", "B"), 3)
  p <- plot_trend_conta(df, facet_by = "Condition")
  expect_true(inherits(p$facet, "FacetWrap"))
})

# --- compare_howdirty ---

test_that("compare_howdirty errors on unnamed files", {
  expect_error(compare_howdirty(c("file1.xlsx", "file2.xlsx")),
               regexp = "named")
})

test_that("compare_howdirty errors on missing files", {
  expect_error(
    compare_howdirty(c(A = "nonexistent_a.xlsx", B = "nonexistent_b.xlsx")),
    regexp = "not found"
  )
})

# --- plot_comparison_conta ---

test_that("plot_comparison_conta errors without Dataset column", {
  expect_error(plot_comparison_conta(make_summ_sample()), regexp = "Dataset")
})

test_that("plot_comparison_conta returns a ggplot", {
  df <- rbind(
    cbind(make_summ_sample(), Dataset = "A"),
    cbind(make_summ_sample(), Dataset = "B")
  )
  df$Dataset <- factor(df$Dataset)
  p <- plot_comparison_conta(df)
  expect_s3_class(p, "ggplot")
})
