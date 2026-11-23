test_that("the selection plot has one tile per variable and method", {
  res <- compare_selection(y ~ ., selection_demo, seed = 1)
  p <- plot(res, truth = c("x1", "x2", "x3"))
  expect_s3_class(p, "ggplot")
  expect_equal(nrow(p$data), 10 * length(default_methods))
  expect_equal(sum(p$data$selected), sum(res$n_selected))
  # most widely selected variables are at the top
  expect_equal(rev(levels(p$data$variable))[1:3], c("x1", "x2", "x3"))
  expect_error(plot(res, truth = "nope"), "not in the data")
})

test_that("the simulation plot has one point per method", {
  sim <- simulate_selection(n = 100, p = 5, n_sim = 4,
                            methods = c("both_bic", "lasso_min"), cores = 1)
  p <- plot(sim)
  expect_s3_class(p, "ggplot")
  expect_equal(nrow(p$data), 2)
})
