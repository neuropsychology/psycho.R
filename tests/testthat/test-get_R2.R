context("get_R2")

test_that("If it works.", {
  # Fit
  library(psycho)

  fit <- lm(Tolerating ~ Adjusting, data=psycho::affective)
  testthat::expect_equal(get_R2(fit))
})
