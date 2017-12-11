context("n_factors")

test_that("Correct Value", {
  results <- attitude %>%
    select_if(is.numeric) %>%
    psycho::n_factors()

  testthat::expect_equal(nrow(summary(results)), 7)
  testthat::expect_equal(nrow(psycho::values(results)$methods), 9)
  testthat::expect_equal(length(plot(results)), 9)
})
