context("analyze.aov")

test_that("If it works.", {
  library(psycho)
  library(lmerTest)
  library(lme4)

  df <- psycho::affective
  x <- aov(Tolerating ~ Salary, data=df)
  testthat::expect_equal(nrow(summary(psycho::analyze(x))), 2)

  x <- anova(lm(Tolerating ~ Salary, data=df))
  testthat::expect_equal(nrow(summary(psycho::analyze(x))), 2)

  x <- aov(Tolerating ~ Birth_Season + Error(Sex), data=df)
  testthat::expect_message(psycho::analyze(x))

  x <- anova(lmerTest::lmer(Tolerating ~ Birth_Season + (1|Sex), data=df))
  testthat::expect_equal(nrow(summary(psycho::analyze(x))), 1)

  x <- anova(lme4::lmer(Tolerating ~ Birth_Season + (1|Sex), data=df))
  testthat::expect_error(psycho::analyze(x))
})
