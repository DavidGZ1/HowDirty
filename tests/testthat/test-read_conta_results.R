row_invariant  <- '"PEG","PEG01",S1,1.2,1000,10000'
row_english    <- '"PEG","PEG01",S1,1.2,1000,10000'
row_na_area    <- '"PEG","PEG01",S1,#N/A,#N/A,10000'

write_skyline_csv <- function(header, row) {
  f <- tempfile(fileext = ".csv")
  writeLines(c(header, row), f)
  f
}

test_that("reads Invariant format (no spaces)", {
  f <- write_skyline_csv(
    "Protein,Peptide,ReplicateName,PeptideRetentionTime,TotalAreaMs1,TicArea",
    row_invariant
  )
  out <- read_conta_results(f, simplify_ContaminantGroup = FALSE)
  expect_s3_class(out, "data.frame")
  expect_true("Abundance" %in% names(out))
  expect_true("ContaminantGroup" %in% names(out))
  expect_true("Contaminant" %in% names(out))
})

test_that("reads English format (spaces in column names)", {
  f <- write_skyline_csv(
    "Protein,Peptide,Replicate Name,Peptide Retention Time,Total Area MS1,Total Ion Current Area",
    row_english
  )
  out <- read_conta_results(f, simplify_ContaminantGroup = FALSE)
  expect_true("Abundance" %in% names(out))
})

test_that("reads dot-separated format", {
  f <- write_skyline_csv(
    "Protein,Peptide,Replicate.Name,Peptide.Retention.Time,Total.Area.MS1,Total.Ion.Current.Area",
    row_english
  )
  out <- read_conta_results(f, simplify_ContaminantGroup = FALSE)
  expect_true("Abundance" %in% names(out))
})

test_that("NA TotalAreaMS1 is zero-filled", {
  f <- write_skyline_csv(
    "Protein,Peptide,Replicate Name,Peptide Retention Time,Total Area MS1,Total Ion Current Area",
    row_na_area
  )
  out <- read_conta_results(f, simplify_ContaminantGroup = FALSE)
  expect_equal(out$TotalAreaMS1[1], 0)
})

test_that("Abundance is TotalAreaMS1 / TotalIonCurrentArea", {
  f <- write_skyline_csv(
    "Protein,Peptide,Replicate Name,Peptide Retention Time,Total Area MS1,Total Ion Current Area",
    row_english
  )
  out <- read_conta_results(f, simplify_ContaminantGroup = FALSE)
  expect_equal(out$Abundance[1], signif(1000 / 10000, 4))
})

test_that("PEG single-digit names are zero-padded", {
  f <- write_skyline_csv(
    "Protein,Peptide,Replicate Name,Peptide Retention Time,Total Area MS1,Total Ion Current Area",
    '"PEG","PEG1",S1,1.2,1000,10000'
  )
  out <- read_conta_results(f, simplify_ContaminantGroup = FALSE)
  expect_true("PEG01" %in% as.character(out$Contaminant))
})

test_that("simplify_ContaminantGroup merges small groups into Others", {
  rows <- paste(
    c('"SmallGroup","C1",S1,1,100,1000',
      '"SmallGroup","C2",S1,1,100,1000',
      '"BigGroup","PEG01",S1,1,100,1000',
      '"BigGroup","PEG02",S1,1,100,1000',
      '"BigGroup","PEG03",S1,1,100,1000',
      '"BigGroup","PEG04",S1,1,100,1000'),
    collapse = "\n"
  )
  f <- tempfile(fileext = ".csv")
  writeLines(c(
    "Protein,Peptide,Replicate Name,Peptide Retention Time,Total Area MS1,Total Ion Current Area",
    rows
  ), f)
  out <- read_conta_results(f, simplify_ContaminantGroup = TRUE)
  expect_true("Others" %in% levels(out$ContaminantGroup))
  expect_false("SmallGroup" %in% levels(out$ContaminantGroup))
})
