test_that("stepwise results match calling step() directly", {
  d <- selection_demo
  full <- y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10
  direct <- function(direction, k) {
    start <- if (direction == "backward") lm(full, d) else lm(y ~ 1, d)
    fit <- step(start, scope = list(lower = ~ 1, upper = full),
                direction = direction, k = k, trace = 0)
    attr(terms(fit), "term.labels")
  }
  res <- compare_selection(y ~ ., d, methods = c("forward_aic", "backward_aic",
                                                 "both_bic", "forward_bic"))
  n <- nrow(d)
  expect_setequal(res$selected$forward_aic, direct("forward", 2))
  expect_setequal(res$selected$backward_aic, direct("backward", 2))
  expect_setequal(res$selected$both_bic, direct("both", log(n)))
  expect_setequal(res$selected$forward_bic, direct("forward", log(n)))
})

test_that("lasso results match calling glmnet directly with the same seed", {
  d <- selection_demo
  x <- as.matrix(d[, paste0("x", 1:10)])
  set.seed(7)
  cv <- glmnet::cv.glmnet(x, d$y, alpha = 1, nfolds = 10)
  nonzero <- function(s) {
    b <- as.matrix(coef(cv, s = s))[-1, 1]
    names(b)[b != 0]
  }
  res <- compare_selection(y ~ ., d, methods = c("lasso_min", "lasso_1se"),
                           seed = 7)
  expect_setequal(res$selected$lasso_min, nonzero("lambda.min"))
  expect_setequal(res$selected$lasso_1se, nonzero("lambda.1se"))
  # a larger penalty never selects more
  expect_lte(res$n_selected[["lasso_1se"]], res$n_selected[["lasso_min"]])
})

test_that("best subset equals an exhaustive search done by hand", {
  set.seed(3)
  d <- simulate_selection_data(n = 80, p = 6, beta = c(1, -1), rho = 0.2)
  vars <- paste0("x", 1:6)
  n <- nrow(d)
  best_by_hand <- function(penalty) {
    best <- NULL
    best_value <- Inf
    for (size in 0:6) {
      subsets <- if (size == 0) list(character(0)) else
        utils::combn(vars, size, simplify = FALSE)
      for (s in subsets) {
        f <- if (length(s) == 0) y ~ 1 else
          reformulate(s, response = "y")
        rss <- sum(resid(lm(f, d))^2)
        value <- n * log(rss / n) + penalty * (length(s) + 1)
        if (value < best_value - 1e-9) {
          best_value <- value
          best <- s
        }
      }
    }
    best
  }
  res <- compare_selection(y ~ ., d, methods = c("best_aic", "best_bic"))
  expect_setequal(res$selected$best_aic, best_by_hand(2))
  expect_setequal(res$selected$best_bic, best_by_hand(log(n)))
})

test_that("the table has one row per variable and one column per method", {
  res <- compare_selection(y ~ ., selection_demo, seed = 1)
  expect_equal(nrow(res$table), 10)
  expect_equal(names(res$table), c("variable", default_methods))
  expect_equal(res$table$variable, paste0("x", c(1:10)))
  expect_equal(unname(res$n_selected),
               unname(colSums(res$table[, default_methods])))
  expect_equal(unname(res$n_methods),
               unname(rowSums(res$table[, default_methods])))
})

test_that("true variables are found in strong-signal data by every method", {
  set.seed(11)
  d <- simulate_selection_data(n = 400, p = 8, beta = c(3, -3, 3), sigma = 1)
  res <- compare_selection(y ~ ., d, methods = all_methods, seed = 1)
  for (m in all_methods) {
    expect_true(all(c("x1", "x2", "x3") %in% res$selected[[m]]), info = m)
  }
})

test_that("the same seed gives the same result", {
  a <- compare_selection(y ~ ., selection_demo, seed = 5)
  b <- compare_selection(y ~ ., selection_demo, seed = 5)
  expect_equal(a$table, b$table)
})

test_that("rows with missing values are dropped for every method", {
  d <- selection_demo
  d$x4[1:5] <- NA
  d$y[6] <- NA
  res <- compare_selection(y ~ ., d, methods = c("both_bic", "lasso_min"),
                           seed = 1)
  expect_equal(res$n_dropped, 6)
  expect_equal(res$n, 194)
  # columns not in the formula do not cause rows to be dropped
  d$unused <- NA
  res2 <- compare_selection(y ~ x1 + x2 + x3, d, methods = "both_bic")
  expect_equal(res2$n_dropped, 1)
})

test_that("a factor is one variable, selected if any dummy column is", {
  set.seed(2)
  n <- 300
  g <- factor(sample(c("a", "b", "c"), n, replace = TRUE))
  x <- rnorm(n)
  noise <- rnorm(n)
  y <- 3 * (g == "c") + x + rnorm(n)
  d <- data.frame(y, g, x, noise)
  res <- compare_selection(y ~ ., d, methods = c("both_bic", "lasso_min",
                                                 "best_bic"), seed = 1)
  expect_equal(res$table$variable, c("g", "x", "noise"))
  expect_true(all(unlist(res$table[res$table$variable == "g", -1])))
  expect_true(all(unlist(res$table[res$table$variable == "x", -1])))
})

test_that("binary outcomes work with stepwise and the lasso", {
  set.seed(9)
  d <- simulate_selection_data(n = 800, p = 6, beta = c(1.5, -1.5),
                               family = "binomial")
  res <- compare_selection(y ~ ., d, methods = c("both_bic", "lasso_min"),
                           family = "binomial", seed = 1)
  expect_true(all(c("x1", "x2") %in% res$selected$both_bic))
  expect_true(all(c("x1", "x2") %in% res$selected$lasso_min))

  # a two level factor outcome gives the same answer as 0/1
  d2 <- d
  d2$y <- factor(ifelse(d$y == 1, "case", "control"), levels = c("control", "case"))
  res2 <- compare_selection(y ~ ., d2, methods = "both_bic", family = "binomial")
  expect_setequal(res2$selected$both_bic, res$selected$both_bic)
})

test_that("the default methods leave out best subset for binary outcomes", {
  set.seed(9)
  d <- simulate_selection_data(n = 400, p = 5, beta = c(1.5, -1.5),
                               family = "binomial")
  res <- compare_selection(y ~ ., d, family = "binomial", seed = 1)
  expect_equal(res$methods, c("forward_aic", "backward_aic", "both_bic",
                              "lasso_min", "lasso_1se"))
  gauss <- compare_selection(y ~ ., selection_demo, seed = 1)
  expect_equal(gauss$methods, default_methods)
})

test_that("bad input gives a readable error", {
  d <- selection_demo
  expect_error(compare_selection("y ~ .", d), "must be a formula")
  expect_error(compare_selection(y ~ ., 1:3), "must be a data frame")
  expect_error(compare_selection(y ~ ., d, methods = "forward"),
               "Unknown method")
  expect_error(compare_selection(y ~ ., d, methods = c("both_bic", "both_bic")),
               "must not repeat")
  expect_error(compare_selection(y ~ ., d, family = "poisson"),
               "must be \"gaussian\" or \"binomial\"")
  expect_error(compare_selection(y ~ ., d, nfolds = 2), "at least 3")
  expect_error(compare_selection(y ~ 1, d), "no predictors")
  expect_error(compare_selection(y ~ x1 * x2, d), "Interactions")
  expect_error(compare_selection(log(y) ~ ., d), "single column name")
  expect_error(compare_selection(z ~ ., d), "object 'z' not found|no column")
  expect_error(compare_selection(y ~ ., d, family = "binomial"),
               "0/1, TRUE/FALSE")
  d$ychar <- "a"
  expect_error(compare_selection(ychar ~ x1 + x2, d), "must be numeric")
  expect_error(compare_selection(y ~ ., selection_demo[1:5, ]), "Fewer than 10")
})

test_that("best subset is refused for binary outcomes and for too many columns", {
  set.seed(1)
  d <- simulate_selection_data(n = 200, p = 4, beta = 1, family = "binomial")
  expect_error(compare_selection(y ~ ., d, methods = "best_bic",
                                 family = "binomial"), "only available")
  wide <- simulate_selection_data(n = 100, p = 30, beta = 1)
  expect_error(compare_selection(y ~ ., wide, methods = "best_bic"),
               "limited to 25")
})

test_that("the print method runs and returns its input", {
  res <- compare_selection(y ~ ., selection_demo, seed = 1)
  expect_output(returned <- print(res), "Variable selection on 200 rows")
  expect_identical(returned, res)
})
