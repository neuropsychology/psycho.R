context("get_predicted.stanreg")

test_that("If it works.", {
  # Fit
  require(rstanarm)


  fit <- rstanarm::stan_glm(
    vs ~ mpg,
    data = mtcars,
    family = binomial(link = "logit"),
    seed = 666
  )
  data <- get_predicted(fit, posterior_predict = T)
  r <- as.numeric(cor.test(data$vs, data$pred_vs)$estimate)
  testthat::expect_equal(r, 0.6, tolerance = 0.2)




  fit <- rstanarm::stan_glm(
    cyl ~ mpg,
    data = mtcars,
    seed = 666
  )
  data <- get_predicted(fit, posterior_predict = T)
  r <- as.numeric(cor.test(data$cyl, data$pred_cyl)$estimate)
  testthat::expect_equal(r, 0.85, tolerance = 0.02)



  fit <- rstanarm::stan_glm(
    Sepal.Length ~ Sepal.Width + Species,
    data = iris,
    seed = 666
  )
  data <- get_predicted(fit, posterior_predict = TRUE)
  r <- as.numeric(cor.test(data$Sepal.Length, data$pred_Sepal.Length)$estimate)
  testthat::expect_equal(r, 0.84, tolerance = 0.02)


  # Actual test -------------------------------------------------------------

  df <- psycho::affective
  fit <- rstanarm::stan_glm(Life_Satisfaction ~ Tolerating, data = df)
  ref_grid <- emmeans::ref_grid(fit, at = list(
    Tolerating = seq(min(df$Tolerating),
      max(df$Tolerating),
      length.out = 10
    )
  ))

  predicted <- psycho::get_predicted(fit, refgrid = ref_grid)
  testthat::expect_equal(mean(predicted$pred_Life_Satisfaction), 4.77, tolerance = 0.05)

  predicted <- psycho::get_predicted(fit, refgrid = ref_grid, keep_iterations=TRUE)
  testthat::expect_equal(length(predicted), 4004)
})
