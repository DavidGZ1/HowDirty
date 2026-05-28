make_conta_annotated <- function() {
  data.frame(
    ContaminantGroup = c("PEG", "PEG", "CHAPS", "CHAPS"),
    Contaminant      = c("PEG01", "PEG01", "CHAPS", "CHAPS"),
    ReplicateName    = c("S1", "S2", "S1", "S2"),
    Sample           = c("Sample1", "Sample2", "Sample1", "Sample2"),
    Condition        = c("A", "A", "A", "A"),
    Abundance        = c(0.1, 0.0, 0.2, 0.3),
    RiskLevel        = factor(c(2, 0, 3, 4), levels = 0:6),
    Risk             = factor(c("2) Low", "0) Not Detected", "3) Medium", "4) High")),
    stringsAsFactors = FALSE
  )
}

test_that("returns a ggplot object", {
  p <- plot_heatmap_conta(make_conta_annotated())
  expect_s3_class(p, "ggplot")
})

test_that("facet_by adds facet layer", {
  p <- plot_heatmap_conta(make_conta_annotated(), facet_by = "ContaminantGroup")
  expect_true(inherits(p$facet, "FacetWrap"))
})

test_that("custom x and y axes are accepted", {
  expect_no_error(plot_heatmap_conta(make_conta_annotated(), x = Sample, y = ContaminantGroup))
})
