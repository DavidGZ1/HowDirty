make_conta <- function() {
  data.frame(
    ContaminantGroup = c("PEG", "PEG", "CHAPS"),
    Contaminant      = c("PEG01", "PEG01", "CHAPS"),
    ReplicateName    = c("S1", "S2", "S1"),
    Abundance        = c(0.1, 0.3, 0.2),
    stringsAsFactors = FALSE
  )
}

test_that("returns expected summary columns", {
  out <- summarize_conta(make_conta(), ContaminantGroup)
  expected <- c("ContaminantGroup",
                "Abundance_min", "Abundance_quantile25", "Abundance_median",
                "Abundance_quantile75", "Abundance_quantile90",
                "Abundance_max", "Abundance_total")
  expect_true(all(expected %in% names(out)))
})

test_that("groups correctly by supplied variable", {
  out <- summarize_conta(make_conta(), ContaminantGroup)
  expect_equal(nrow(out), 2)
})

test_that("Abundance_total is sum of group abundances", {
  out <- summarize_conta(make_conta(), ContaminantGroup)
  peg_total <- out$Abundance_total[out$ContaminantGroup == "PEG"]
  expect_equal(peg_total, signif(0.1 + 0.3, 4))
})

test_that("all-zero abundance produces zero totals", {
  df <- make_conta()
  df$Abundance <- 0
  out <- summarize_conta(df, ContaminantGroup)
  expect_true(all(out$Abundance_total == 0))
})

test_that("single-row dataframe does not error", {
  df <- make_conta()[1, ]
  expect_no_error(summarize_conta(df, ContaminantGroup))
})

test_that("empty dataframe returns zero rows without error", {
  df <- make_conta()[0, ]
  out <- summarize_conta(df, ContaminantGroup)
  expect_equal(nrow(out), 0)
})

test_that("supports multiple grouping variables", {
  out <- summarize_conta(make_conta(), ContaminantGroup, ReplicateName)
  expect_true("ReplicateName" %in% names(out))
  expect_equal(nrow(out), 3)
})
