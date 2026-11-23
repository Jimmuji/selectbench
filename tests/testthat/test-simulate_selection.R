test_that("simulated data have the right shape and true variables", {
  set.seed(1)
  d <- simulate_selection_data(n = 150, p = 7, beta = c(2, -1))
  expect_equal(dim(d), c(150, 8))
  expect_equal(names(d), c("y", paste0("x", 1:7)))
  expect_equal(attr(d, "truth"), c("x1", "x2"))
})

test_that("the outcome follows the stated model", {
  set.seed(1)
  d <- simulate_selection_data(n = 20000, p = 5, beta = c(2, -1.5, 1),
                               sigma = 1, intercept = 0.5)
  fit <- lm(y ~ ., d)
  expect_equal(unname(coef(fit)[c("(Intercept)", "x1", "x2", "x3", "x4")]),
               c(0.5, 2, -1.5, 1, 0), tolerance = 0.05)
  expect_equal(sigma(fit), 1, tolerance = 0.03)
})

test_that("predictors have the requested correlation", {
  set.seed(1)
  d <- simulate_selection_data(n = 20000, p = 4, beta = 1, rho = 0.5)
  expect_equal(cor(d$x1, d$x4), 0.5, tolerance = 0.03)
  expect_equal(cor(d$x2, d$x3), 0.5, tolerance = 0.03)
  d0 <- simulate_selection_data(n = 20000, p = 4, beta = 1, rho = 0)
  expect_equal(cor(d0$x1, d0$x4), 0, tolerance = 0.03)
  expect_equal(sd(d$x1), 1, tolerance = 0.03)
})

test_that("binary outcomes are 0 or 1", {
  set.seed(1)
  d <- simulate_selection_data(n = 500, p = 4, beta = 2, family = "binomial")
  expect_true(all(d$y %in% c(0, 1)))
  expect_gt(mean(d$y), 0.2)
  expect_lt(mean(d$y), 0.8)
})

test_that("bad input gives a readable error", {
  expect_error(simulate_selection_data(n = 5), "at least 10")
  expect_error(simulate_selection_data(p = 1), "at least 2")
  expect_error(simulate_selection_data(p = 3, beta = c(1, 1, 1, 1)),
               "non-zero effects")
  expect_error(simulate_selection_data(beta = c(1, 0)), "non-zero effects")
  expect_error(simulate_selection_data(rho = 1), "less than 1")
  expect_error(simulate_selection_data(sigma = 0), "positive")
})

test_that("simulation summary is consistent with the runs", {
  sim <- simulate_selection(n = 300, p = 6, beta = c(3, -3, 3), n_sim = 12,
                            methods = c("both_bic", "best_bic"), cores = 1)
  expect_equal(sim$summary$method, c("both_bic", "best_bic"))
  expect_equal(nrow(sim$runs), 24)
  both <- sim$runs[sim$runs$method == "both_bic", ]
  expect_equal(sim$summary$sensitivity[1], mean(both$sensitivity))
  expect_equal(sim$summary$exact[1], mean(both$exact))
  expect_equal(sim$summary$mcse_exact[1],
               sqrt(mean(both$exact) * (1 - mean(both$exact)) / 12))
  # strong signal and BIC: the true variables are always found
  expect_equal(sim$summary$sensitivity, c(1, 1))
  expect_gt(min(sim$summary$exact), 0.6)
})

test_that("results do not depend on the number of cores", {
  one <- simulate_selection(n = 150, p = 5, n_sim = 6, methods = "both_bic",
                            cores = 1, seed = 4)
  two <- simulate_selection(n = 150, p = 5, n_sim = 6, methods = "both_bic",
                            cores = 2, seed = 4)
  expect_equal(one$runs, two$runs)
})

test_that("more noise in the data makes selection harder", {
  easy <- simulate_selection(n = 300, p = 6, beta = c(3, -3, 3), sigma = 1,
                             n_sim = 20, methods = "both_bic", cores = 1)
  hard <- simulate_selection(n = 300, p = 6, beta = c(0.3, -0.3, 0.3),
                             sigma = 3, n_sim = 20, methods = "both_bic",
                             cores = 1)
  expect_gt(easy$summary$sensitivity, hard$summary$sensitivity)
})

test_that("the print method runs", {
  sim <- simulate_selection(n = 100, p = 4, n_sim = 3, methods = "both_bic",
                            cores = 1)
  expect_output(print(sim), "Simulation: 3 data sets")
})
