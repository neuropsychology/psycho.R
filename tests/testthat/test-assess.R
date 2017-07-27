context("assess")

test_that("Correct percentile", {
  percentile <- assess(1.3)
  percentile <- percentile$values
  percentile <- round(percentile$percentile, 1)
  expect_equal(percentile, 0.9)
})
