context("analyze.aov")

test_that("If it works.", {
  library(psycho)
  library(psycho)

  df <- psycho::affective
  x <- aov(df$Tolerating ~ df$Sex)

  rez <- psycho::analyze(x)
  testthat::expect_equal(nrow(summary(rez)), 2)
})
