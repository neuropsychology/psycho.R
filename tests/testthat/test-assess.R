context("assess")

test_that("It works", {
  # Correct percentile
  percentile <- psycho::assess(1.3)
  percentile <- percentile$values
  percentile <- round(percentile$percentile, 1)
  testthat::expect_equal(percentile, 0.9)

  # Correct sanity check
  percentile <- psycho::assess(c(-0.2, 2))
  percentile <- psycho::values(percentile)$z_score
  testthat::expect_equal(percentile[1], -0.2)
})
