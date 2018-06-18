context("get_R2")

test_that("If it works.", {
  # Fit
  library(psycho)

  fit <- lm(Tolerating ~ Adjusting, data = psycho::affective)
  testthat::expect_equal(get_R2(fit)$R2, 0.08, tol = 0.01)

  fit <- glm(Sex ~ Adjusting, data = psycho::affective, family = "binomial")
  testthat::expect_equal(get_R2(fit), 0.025, tol = 0.01)

  fit <- lmerTest::lmer(Tolerating ~ Adjusting + (1 | Sex), data = psycho::affective)
  testthat::expect_equal(get_R2(fit)$R2m, 0.08, tol = 0.01)

  fit <- lme4::glmer(Sex ~ Adjusting + (1 | Salary), data = na.omit(psycho::affective), family = "binomial")
  testthat::expect_equal(get_R2(fit)$R2m, 0.037, tol = 0.01)
})
