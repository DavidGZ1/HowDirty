test_that("returns required columns with no arguments", {
  out <- get_annotation_template(save = FALSE)
  expect_true(all(c("ReplicateNameSkyline", "Sample", "Condition", "DilutionFactor") %in% names(out)))
})

test_that("extra_cols adds requested columns", {
  out <- get_annotation_template(save = FALSE, extra_cols = c("Batch", "Instrument"))
  expect_true("Batch" %in% names(out))
  expect_true("Instrument" %in% names(out))
})

test_that("extra columns are preserved through read_samples_annotation", {
  f <- tempfile(fileext = ".csv")
  write.csv(
    data.frame(ReplicateNameSkyline = "S1", Sample = "S1",
               Condition = "A", DilutionFactor = 1, Batch = "B1"),
    f, row.names = FALSE
  )
  out <- read_samples_annotation(f)
  expect_true("Batch" %in% names(out))
  expect_equal(as.character(out$Batch[1]), "B1")
})

test_that("extra columns survive annotate_conta_samples join", {
  annot <- data.frame(
    ReplicateName  = "S1",
    Sample         = "S1",
    Condition      = "A",
    DilutionFactor = 1,
    Batch          = "B1",
    stringsAsFactors = FALSE
  )
  conta <- data.frame(
    ContaminantGroup    = "PEG",
    Contaminant         = "PEG01",
    ReplicateName       = "S1",
    TotalAreaMS1        = 1000,
    TotalIonCurrentArea = 10000,
    Abundance           = 0.1,
    stringsAsFactors    = FALSE
  )
  out <- annotate_conta_samples(conta, annot)
  expect_true("Batch" %in% names(out))
  expect_equal(as.character(out$Batch[1]), "B1")
})

test_that("overwrite = FALSE stops if file exists", {
  tmp_dir <- tempdir()
  old_wd  <- setwd(tmp_dir)
  on.exit(setwd(old_wd), add = TRUE)
  out_file <- file.path(tmp_dir, "samples_annotation_template.csv")
  writeLines("", out_file)
  on.exit(unlink(out_file), add = TRUE)
  expect_error(
    get_annotation_template(save = TRUE, overwrite = FALSE),
    regexp = "already exists"
  )
})
