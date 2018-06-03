context("bayes_cor")

test_that("Correct Value", {

  results <- psycho::bayes_cor.test(psycho::affective$Concealing,
                                    psycho::affective$Tolerating)

  testthat::expect_equal(results$values$median, 0.073, tol = 0.05)
  testthat::expect_equal(results$values$effect_size$values$`very weak`, 0.995, tol = 0.05)

  results <- psycho::bayes_cor(iris)
  testthat::expect_equal(nrow(results$values$r), 4)


  results <- psycho::bayes_cor(
    dplyr::select(iris, dplyr::starts_with("Sepal")),
    dplyr::select(iris, dplyr::starts_with("Petal"))
  )
  testthat::expect_equal(nrow(results$values$r), 2)
})
