context("dprime")


test_that("dprime", {
  testthat::expect_equal(dprime(9, 2, 1, 7)$dprime, 1.65, tolerance = 0.1)
  testthat::expect_equal(dprime(1, 9, 1, 0)$dprime, -1.49, tolerance = 0.1)

  df <- data.frame(
    Participant = c("A", "B", "C"),
    n_hit = c(1, 2, 5),
    n_fa = c(6, 8, 1)
  )

  indices <- dprime(n_hit = df$n_hit, n_fa = df$n_fa, n_targets = 10, n_distractors = 10, adjusted = F)
  testthat::expect_equal(indices$dprime[1], -1.53, tolerance = 0.1)

  testthat::expect_equal(dprime(5, 0, n_targets = 10, n_distractors = 8, adjusted = FALSE)$aprime, 0.875, tolerance = 0.1)
})
