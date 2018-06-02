context("bayesian_cor")

test_that("Correct Value", {
  x <- psycho::affective$Concealing
  y <- psycho::affective$Tolerating

  results <- psycho::bayesian_cor.test(x, y)

  testthat::expect_equal(results$values$median, 0.073, tol = 0.05)
  testthat::expect_equal(results$values$effect_size$values$`very weak`, 0.995, tol = 0.05)

  results <- psycho::bayesian_cor(iris)
  testthat::expect_equal(nrow(results$values$r), 4)


  results <- psycho::bayesian_cor(
    dplyr::select(iris, dplyr::starts_with("Sepal")),
    dplyr::select(iris, dplyr::starts_with("Petal"))
  )
  testthat::expect_equal(nrow(results$values$r), 2)
})
