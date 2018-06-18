context("analyze.glm")

test_that("If it works.", {
  library(psycho)

  # GLM
  fit <- glm(vs ~ mpg, data = mtcars, family = "binomial")

  model <- analyze(fit)
  values <- values(model)
  testthat::expect_equal(round(values$effects$mpg$Coef, 2), 0.43, tolerance = 0.02)

  # test summary
  summa <- summary(model, round = 2)
  testthat::expect_equal(nrow(summa), 2)

})
