context("get_predicted.stanreg")

test_that("If it works.", {
  # Fit
  require(rstanarm)


  fit <- rstanarm::stan_glm(
    vs ~ mpg,
    data = mtcars,
    family = binomial(link = "logit"),
    seed=666
  )
  data <- get_predicted(fit)
  r <- as.numeric(cor.test(data$vs, data$pred_vs_median)$estimate)
  testthat::expect_equal(r, 0.6, tolerance=0.2)


  data_new <- get_predicted(fit, newdf=T)
  testthat::expect_equal(mean(data_new$pred_vs_probability), 0.56, tolerance=0.05)



  fit <- rstanarm::stan_glm(
    cyl ~ mpg,
    data = mtcars,
    seed=666
  )
  data <- get_predicted(fit)
  r <- as.numeric(cor.test(data$cyl, data$pred_cyl_median)$estimate)
  testthat::expect_equal(r, 0.84, tolerance=0.02)


  data_new <- get_predicted(fit, newdf=T)
  testthat::expect_equal(mean(data_new$pred_cyl_median), 5.66, tolerance=0.05)


})
