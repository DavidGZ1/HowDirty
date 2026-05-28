make_conta <- function() {
  data.frame(
    ContaminantGroup   = "PEG",
    Contaminant        = "PEG01",
    ReplicateName      = c("S1", "S2"),
    TotalAreaMS1       = c(1000, 2000),
    TotalIonCurrentArea = c(10000, 10000),
    Abundance          = c(0.1, 0.2),
    stringsAsFactors   = FALSE
  )
}

make_annot <- function() {
  data.frame(
    ReplicateName  = c("S1", "S2"),
    Sample         = c("Sample1", "Sample2"),
    Condition      = c("A", "A"),
    DilutionFactor = c(1, 1),
    stringsAsFactors = FALSE
  )
}

test_that("joins annotation to contaminant results", {
  out <- annotate_conta_samples(make_conta(), make_annot())
  expect_true("Condition" %in% names(out))
  expect_true("Sample" %in% names(out))
  expect_equal(nrow(out), 2)
})

test_that("samples missing from annotation are dropped (inner join)", {
  annot <- make_annot()[1, ]
  out <- annotate_conta_samples(make_conta(), annot)
  expect_equal(nrow(out), 1)
})

test_that("remove_missing drops all-zero contaminants", {
  df <- make_conta()
  df$TotalAreaMS1[df$ReplicateName == "S1"] <- 0
  df$Abundance[df$ReplicateName == "S1"]    <- 0
  out <- annotate_conta_samples(df, make_annot(), remove_missing = TRUE)
  expect_equal(nrow(out), 1)
  expect_equal(out$ReplicateName[1], "S2")
})

test_that("multiply_dilution_factor scales Abundance", {
  annot <- make_annot()
  annot$DilutionFactor <- c(2, 2)
  df <- make_conta()
  out <- annotate_conta_samples(df, annot, multiply_dilution_factor = TRUE)
  expect_equal(out$Abundance[1], signif(0.1 * 2, 4))
})
