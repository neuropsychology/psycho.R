context("get_predicted")

test_that("If it works.", {



  # Rstanarm ----------------------------------------------------------------
  library(psycho)
  require(rstanarm)


  fit <- rstanarm::stan_glm(
    vs ~ mpg,
    data = mtcars,
    family = binomial(link = "logit"),
    seed = 666
  )
  data <- psycho::get_predicted(fit)
  r <- as.numeric(cor.test(data$vs, data$vs_Median)$estimate)
  testthat::expect_equal(r, 0.68, tolerance = 0.2)




  fit <- rstanarm::stan_glm(
    cyl ~ mpg,
    data = mtcars,
    seed = 666
  )
  data <- psycho::get_predicted(fit)
  r <- as.numeric(cor.test(data$cyl, data$cyl_Median)$estimate)
  testthat::expect_equal(r, 0.85, tolerance = 0.02)



  fit <- rstanarm::stan_glm(
    Sepal.Length ~ Sepal.Width + Species,
    data = iris,
    seed = 666
  )
  data <- psycho::get_predicted(fit, posterior_predict = TRUE)
  r <- as.numeric(cor.test(data$Sepal.Length, data$Sepal.Length_Median)$estimate)
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

  predicted <- psycho::get_predicted(fit, newdata = ref_grid)
  testthat::expect_equal(mean(predicted$Life_Satisfaction_Median), 4.77, tolerance = 0.05)

  predicted <- psycho::get_predicted(fit, newdata = ref_grid, keep_iterations = TRUE)
  testthat::expect_equal(length(predicted), 4004)







  # GLM and LM --------------------------------------------------------------

  fit <- glm(vs ~ mpg, data = mtcars, family = binomial(link = "logit"))
  data <- psycho::get_predicted(fit)
  r <- as.numeric(cor.test(data$vs, data$vs_Predicted)$estimate)
  testthat::expect_equal(r, 0.68, tolerance = 0.2)


  fit <- lm(cyl ~ mpg, data = mtcars)
  data <- psycho::get_predicted(fit)
  r <- as.numeric(cor.test(mtcars$cyl, data$cyl_Predicted)$estimate)
  testthat::expect_equal(r, 0.85, tolerance = 0.02)

  # glmerMod ----------------------------------------------------------------
  library(lme4)

  fit <- lme4::glmer(vs ~ mpg + (1 | cyl), data = mtcars, family = binomial(link = "logit"))
  data <- psycho::get_predicted(fit)
  r <- as.numeric(cor.test(data$vs, data$vs_Predicted)$estimate)
  testthat::expect_equal(r, 0.79, tolerance = 0.02)

  fit <- lme4::lmer(Tolerating ~ Adjusting + (1 | Salary), data = affective)
  data <- psycho::get_predicted(fit)
  r <- as.numeric(cor.test(data$Tolerating, data$Tolerating_Predicted)$estimate)
  testthat::expect_equal(r, 0.3, tolerance = 0.02)

  library(lmerTest)
  fit <- lmerTest::lmer(Tolerating ~ Adjusting + (1 | Salary), data = affective)
  data <- psycho::get_predicted(fit)
  r <- as.numeric(cor.test(data$Tolerating, data$Tolerating_Predicted)$estimate)
  testthat::expect_equal(r, 0.3, tolerance = 0.02)
})
