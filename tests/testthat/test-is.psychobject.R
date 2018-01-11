context("is.psychobject")

test_that("is.psychobject", {
  df <- attitude
  results <- psycho::correlation(df)
  testthat::expect_true(psycho::is.psychobject(results))

})
