context("analyze.lavaan")

test_that("If it works.", {
  library(psycho)
  library(lavaan)

  HS.model <- " visual  =~ x1 + x2 + x3\n  textual =~ x4 + x5 + x6\n  speed   =~ x7 + x8 + x9 "

  fit <- lavaan::cfa(HS.model, data = lavaan::HolzingerSwineford1939)
  rez <- analyze(fit)
  testthat::expect_equal(nrow(summary(rez)), 24)
})
