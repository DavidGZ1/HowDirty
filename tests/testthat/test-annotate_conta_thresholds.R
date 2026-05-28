make_threshold <- function() {
  data.frame(
    ContaminantGroup = "PEG",
    Contaminant      = "PEG01",
    Tshd_abundance_quantile25 = 0.01,
    Tshd_abundance_quantile50 = 0.05,
    Tshd_abundance_quantile75 = 0.10,
    Tshd_abundance_quantile90 = 0.20,
    stringsAsFactors = FALSE
  )
}

make_conta <- function(abundance) {
  data.frame(
    ContaminantGroup = "PEG",
    Contaminant      = "PEG01",
    ReplicateName    = "S1",
    Abundance        = abundance,
    stringsAsFactors = FALSE
  )
}

tshd <- make_threshold()

test_that("Abundance == 0 gives RiskLevel 0", {
  out <- annotate_conta_thresholds(make_conta(0), tshd, Abundance)
  expect_equal(as.character(out$Risk[1]), "0) Not Detected")
})

test_that("Abundance below Q25 gives RiskLevel 1", {
  out <- annotate_conta_thresholds(make_conta(0.005), tshd, Abundance)
  expect_equal(as.character(out$Risk[1]), "1) Very Low")
})

test_that("Abundance between Q25 and Q50 gives RiskLevel 2", {
  out <- annotate_conta_thresholds(make_conta(0.03), tshd, Abundance)
  expect_equal(as.character(out$Risk[1]), "2) Low")
})

test_that("Abundance between Q50 and Q75 gives RiskLevel 3", {
  out <- annotate_conta_thresholds(make_conta(0.07), tshd, Abundance)
  expect_equal(as.character(out$Risk[1]), "3) Medium")
})

test_that("Abundance between Q75 and Q90 gives RiskLevel 4", {
  out <- annotate_conta_thresholds(make_conta(0.15), tshd, Abundance)
  expect_equal(as.character(out$Risk[1]), "4) High")
})

test_that("Abundance above Q90 gives RiskLevel 5", {
  out <- annotate_conta_thresholds(make_conta(0.25), tshd, Abundance)
  expect_equal(as.character(out$Risk[1]), "5) Very High")
})

test_that("RiskLevel and Risk columns are factors", {
  out <- annotate_conta_thresholds(make_conta(0.03), tshd, Abundance)
  expect_s3_class(out$RiskLevel, "factor")
  expect_s3_class(out$Risk, "factor")
})

test_that("Threshold columns are removed from output", {
  out <- annotate_conta_thresholds(make_conta(0.03), tshd, Abundance)
  expect_false(any(grepl("^Tshd_", names(out))))
})

test_that("errors when neither Contaminant nor ContaminantGroup present", {
  bad_df <- data.frame(Abundance = 0.1)
  expect_error(annotate_conta_thresholds(bad_df, tshd, Abundance))
})
