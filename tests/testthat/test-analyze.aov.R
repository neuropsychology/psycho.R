context("analyze.aov")

test_that("If it works.", {
  library(psycho)

  df <- psycho::affective
  x <- aov(df$Tolerating ~ df$Salary)
  testthat::expect_equal(nrow(summary(psycho::analyze(x))), 2)

  x <- anova(lm(df$Tolerating ~ df$Salary))
  testthat::expect_equal(nrow(summary(psycho::analyze(x))), 2)

  x <- aov(df$Tolerating ~ df$Birth_Season + Error(df$Sex))
  testthat::expect_equal(nrow(summary(psycho::analyze(x))), 2)

  x <- anova(lmerTest::lmer(df$Tolerating ~ df$Birth_Season + (1|df$Sex)))
  testthat::expect_equal(nrow(summary(psycho::analyze(x))), 1)

  x <- anova(lme4::lmer(df$Tolerating ~ df$Birth_Season + (1|df$Sex)))
  testthat::expect_error(psycho::analyze(x))
})
