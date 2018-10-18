context("get_contrasts")

test_that("If it works.", {
  # rstanarm
  require(rstanarm)

  df <- psycho::affective
  fit <- rstanarm::stan_glm(Life_Satisfaction ~ Salary, data = df)

  contrasts <- psycho::get_contrasts(fit, "Salary")
  testthat::expect_equal(mean(contrasts$Median), -0.134, tolerance = 0.05)

  # lmerTest
  require(lmerTest)

  fit <- lmerTest::lmer(Adjusting ~ Birth_Season + (1 | Salary), data = psycho::affective)

  contrasts <- get_contrasts(fit)
  testthat::expect_equal(mean(contrasts$Difference), -0.218, tolerance = 0.05)

  # glmer
  require(lme4)

  fit <- lme4::glmer(Sex ~ Birth_Season + (1 | Salary), data = psycho::affective, family = "binomial")

  contrasts <- get_contrasts(fit, adjust="bonf")
  testthat::expect_equal(mean(contrasts$Difference), -0.0734, tolerance = 0.05)

  # glm
  fit <- glm(Sex ~ Birth_Season, data = psycho::affective, family = "binomial")

  contrasts <- get_contrasts(fit)
  testthat::expect_equal(mean(contrasts$Difference), -0.0458, tolerance = 0.05)
})
