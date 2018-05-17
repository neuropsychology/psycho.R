context("odds_to_probs")

test_that("Correct", {
  testthat::expect_equal(odds_to_probs(-1.6), 0.17, tolerance = 0.01)
  testthat::expect_equal(odds_to_probs(-1.6, log = F), 2.66, tolerance = 0.01)

  testthat::expect_equal(
    ncol(odds_to_probs(
      psycho::affective,
      subset = c("Life_Satisfaction"),
      except = c("Sex")
    )),
    8
  )
})
