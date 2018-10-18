context("get_means")

test_that("If it works.", {
  # rstanarm
  require(rstanarm)

  df <- psycho::affective
  fit <- rstanarm::stan_glm(Life_Satisfaction ~ Salary, data = df)

  means <- psycho::get_means(fit, "Salary")
  testthat::expect_equal(mean(means$Median), 4.876, tolerance = 0.05)


  # lmerTest
  require(lmerTest)

  fit <- lmerTest::lmer(Adjusting ~ Birth_Season + (1 | Salary), data = psycho::affective)

  means <- get_means(fit, formula = "Birth_Season")
  testthat::expect_equal(mean(means$Mean), 3.860, tolerance = 0.05)


  # glmer
  require(lme4)

  fit <- lme4::glmer(Sex ~ Birth_Season + (1 | Salary), data = psycho::affective, family = "binomial")

  means <- get_means(fit, formula = "Birth_Season")
  testthat::expect_equal(mean(means$Mean), -1.221759, tolerance = 0.05)

  # glm
  fit <- glm(Sex ~ Birth_Season, data = psycho::affective, family = "binomial")

  means <- get_means(fit, formula = "Birth_Season")
  testthat::expect_equal(mean(means$Mean), -1.413, tolerance = 0.05)

})
