context("deprecated")




test_that("find_combinations.formula", {
  f <- as.formula("Y ~ A + B + C + D + (1|E)")
  combinations <- find_combinations(f)
  testthat::expect_equal(length(combinations), 32)
})




test_that("find_matching_string", {
  testthat::expect_equal(find_matching_string("Hwo rea ouy", c("How are you", "Not this word", "Nice to meet you")), "How are you")
})






test_that("find_season", {
  dates <- c("2017-02-15", "2017-05-15", "2017-08-15", "2017-11-15")
  dates <- find_season(dates)
  expect_equal(as.character(dates[1]), "Winter")
})






















test_that("plot.psychobject", {
  output <- list(plot = 1)
  class(output) <- c("psychobject", "list")
  plot <- plot(output)
  expect_equal(plot, 1)
})







test_that("power_analysis", {
  fit <- lm(Sepal.Length ~ Sepal.Width, data = iris)
  results <- psycho::power_analysis(fit, n_max = 300, n_min = 150, step = 50, n_batch = 1)

  testthat::expect_equal(nrow(results), 8, tolerance = 0.01)
})







test_that("print.psychobject", {
  output <- list(text = 1)
  class(output) <- c("psychobject", "list")
  text <- print(output)
  expect_equal(text, 1)
})









test_that("remove_empty_cols", {
  df <- data.frame(
    A = c(1, 2, 3),
    B = c(1, 2, 3)
  )
  df$C <- NA

  testthat::expect_equal(ncol(psycho::remove_empty_cols(df)), 2)
})









test_that("values.psychobject", {
  output <- list(values = 1)
  class(output) <- c("psychobject", "list")
  values <- values(output)
  expect_equal(values, 1)
})




