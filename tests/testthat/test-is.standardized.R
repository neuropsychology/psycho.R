context("standardize")

test_that("Correct Value", {
  df <- psycho::affective
  testthat::expect_equal(is.standardized(df), F)
  df <- psycho::standardize(df)
  testthat::expect_equal(is.standardized(df), T)
})
