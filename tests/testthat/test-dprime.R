context("dprime")

test_that("Correct Value", {
  testthat::expect_equal(dprime(9, 2, 1, 7)$dprime, 1.65, tolerance = 0.1)
  testthat::expect_equal(dprime(9, 0, 0, 0)$dprime, 1.74, tolerance = 0.1)
  testthat::expect_equal(dprime(0, 0, 10, 0)$dprime, -1.28, tolerance = 0.1)

  df <- data.frame(
    Participant = c("A", "B", "C"),
    n_hit = c(1, 2, 5),
    n_fa = c(6, 8, 1)
  )

  indices <- dprime(n_hit = df$n_hit, n_fa = df$n_fa, n_targets = 10, n_distractors = 10, adjusted = F)
  testthat::expect_equal(indices$dprime[1], -1.53, tolerance = 0.1)

  testthat::expect_equal(dprime(5, 0, n_targets = 10, n_distractors = 8)$aprime, 0.875, tolerance = 0.1)
})
