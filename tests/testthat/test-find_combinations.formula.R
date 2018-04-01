context("find_combinations.formula")

test_that("Correct", {
  f <- as.formula("Y ~ A + B + C + D + (1|E)")
  combinations <- find_combinations(f)
  testthat::expect_equal(length(combinations), 32)
})
