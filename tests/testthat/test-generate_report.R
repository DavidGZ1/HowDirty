test_that("generate_howdirty_report errors on missing file_peak_areas", {
  expect_error(
    generate_howdirty_report(
      dataset          = "Test",
      file_peak_areas  = "nonexistent.csv",
      file_annotation  = "nonexistent.csv"
    ),
    regexp = "file_peak_areas not found"
  )
})

test_that("generate_howdirty_report errors on missing file_annotation", {
  f <- tempfile(fileext = ".csv")
  file.create(f)
  on.exit(unlink(f))
  expect_error(
    generate_howdirty_report(
      dataset          = "Test",
      file_peak_areas  = f,
      file_annotation  = "nonexistent.csv"
    ),
    regexp = "file_annotation not found"
  )
})

test_that("generate_howdirty_report errors on missing file_ref_thresholds when not FALSE", {
  f <- tempfile(fileext = ".csv")
  file.create(f)
  on.exit(unlink(f))
  expect_error(
    generate_howdirty_report(
      dataset             = "Test",
      file_peak_areas     = f,
      file_annotation     = f,
      file_ref_thresholds = "nonexistent.xlsx"
    ),
    regexp = "file_ref_thresholds not found"
  )
})

test_that("run_howdirty_batch errors on missing required columns", {
  bad_manifest <- data.frame(dataset = "Exp1", stringsAsFactors = FALSE)
  expect_error(run_howdirty_batch(bad_manifest), regexp = "missing required columns")
})

test_that("run_howdirty_batch adds file_ref_thresholds = FALSE when absent", {
  manifest <- data.frame(
    dataset          = "Exp1",
    file_peak_areas  = "nonexistent.csv",
    file_annotation  = "nonexistent.csv",
    stringsAsFactors = FALSE
  )
  # Should fail at file check, not at manifest validation
  result <- run_howdirty_batch(manifest)
  expect_equal(result$status[1], "error")
  expect_equal(result$dataset[1], "Exp1")
})
