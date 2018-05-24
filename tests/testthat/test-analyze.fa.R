context("analyze.fa")

test_that("If it works.", {
  library(psycho)
  library(psych)

  x <- psych::fa(psych::Thurstone.33, 2)

  results <- analyze(x)
  testthat::expect_equal(nrow(summary(results)), 9)

  cfa_model <- get_cfa_model(results$values$loadings, treshold = 0.3)
  testthat::expect_equal(nchar(cfa_model), 195)
})
