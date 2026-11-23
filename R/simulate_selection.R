#' Simulate data in which the true variables are known
#'
#' Generates a data frame with an outcome `y` and `p` predictors `x1` to
#' `xp`. The first `length(beta)` predictors have the effects in `beta` and
#' the remaining predictors have no effect on `y`.
#'
#' @param n Number of rows.
#' @param p Number of predictors.
#' @param beta Effects of the first predictors. The remaining `p` minus
#'   `length(beta)` predictors are noise.
#' @param rho Correlation between every pair of predictors, from 0 to less
#'   than 1. Higher values make selection harder.
#' @param sigma Standard deviation of the error, for `family = "gaussian"`.
#' @param family `"gaussian"` or `"binomial"`.
#' @param intercept Intercept of the linear predictor.
#'
#' @return A data frame with columns `y`, `x1`, ..., `xp`. The names of the
#'   true variables are stored in `attr(, "truth")`.
#'
#' @examples
#' d <- simulate_selection_data(n = 100, p = 8, beta = c(2, -1), rho = 0.3)
#' head(d)
#' attr(d, "truth")
#'
#' @export
simulate_selection_data <- function(n = 200, p = 10, beta = c(2, -1.5, 1),
                                    rho = 0, sigma = 1, family = "gaussian",
                                    intercept = 0) {
  check_family(family)
  if (!is.numeric(n) || length(n) != 1 || n < 10) {
    stop("`n` must be a single number of at least 10.", call. = FALSE)
  }
  if (!is.numeric(p) || length(p) != 1 || p < 2) {
    stop("`p` must be a single number of at least 2.", call. = FALSE)
  }
  if (!is.numeric(beta) || length(beta) < 1 || length(beta) > p ||
      anyNA(beta) || any(beta == 0)) {
    stop("`beta` must hold between 1 and `p` non-zero effects.", call. = FALSE)
  }
  if (!is.numeric(rho) || length(rho) != 1 || rho < 0 || rho >= 1) {
    stop("`rho` must be a single number from 0 to less than 1.", call. = FALSE)
  }
  if (!is.numeric(sigma) || length(sigma) != 1 || sigma <= 0) {
    stop("`sigma` must be a single positive number.", call. = FALSE)
  }

  # equal correlation: every predictor shares one common component
  shared <- stats::rnorm(n)
  x <- sqrt(1 - rho) * matrix(stats::rnorm(n * p), n, p) + sqrt(rho) * shared
  colnames(x) <- paste0("x", seq_len(p))
  eta <- intercept + drop(x[, seq_along(beta), drop = FALSE] %*% beta)

  y <- if (family == "gaussian") {
    eta + stats::rnorm(n, sd = sigma)
  } else {
    stats::rbinom(n, size = 1, prob = stats::plogis(eta))
  }

  out <- data.frame(y = y, x)
  attr(out, "truth") <- colnames(x)[seq_along(beta)]
  out
}

#' Compare selection methods over many simulated data sets
#'
#' Repeats the same experiment many times: generate data with known true
#' variables ([simulate_selection_data()]), run the selection methods
#' ([compare_selection()]), and score each one ([score_selection()]). Because
#' one data set can favour one method by luck, the averages over many data
#' sets are what compare the methods fairly.
#'
#' @inheritParams simulate_selection_data
#' @inheritParams compare_selection
#' @param n_sim Number of simulated data sets.
#' @param seed Seed. Data set `i` is generated with seed `seed + i`, so the
#'   result is the same every time, whatever the number of cores.
#' @param cores Number of cores. The default leaves one core free. On Windows
#'   the simulations run one after another.
#'
#' @return An object of class `selection_sim`, a list with:
#' \describe{
#'   \item{summary}{One row per method: `sensitivity` (the average share of
#'     true variables found), `false_positive_rate` (the average share of
#'     noise variables selected), `exact` (the proportion of data sets in
#'     which the selected set equals the true set), `mcse_exact` (the Monte
#'     Carlo standard error of `exact`), and `n_selected` (the average
#'     number of variables selected).}
#'   \item{runs}{The score of every method in every data set.}
#'   \item{settings}{The values of the arguments that were used.}
#' }
#' Use [plot()] on the result to compare the methods.
#'
#' @examples
#' sim <- simulate_selection(n = 100, p = 8, beta = c(2, -1.5, 1),
#'                           n_sim = 20, cores = 1)
#' sim$summary
#'
#' @export
simulate_selection <- function(n = 200, p = 10, beta = c(2, -1.5, 1),
                               rho = 0, sigma = 1, family = "gaussian",
                               intercept = 0, methods = NULL,
                               n_sim = 200, nfolds = 10, seed = 2025,
                               cores = max(1, parallel::detectCores() - 1)) {
  check_family(family)
  methods <- resolve_methods(methods, family)
  check_methods(methods, family)
  if (!is.numeric(n_sim) || length(n_sim) != 1 || n_sim < 1) {
    stop("`n_sim` must be a single positive number.", call. = FALSE)
  }
  if (!is.numeric(cores) || length(cores) != 1 || cores < 1) {
    stop("`cores` must be a single positive number.", call. = FALSE)
  }
  if (.Platform$OS.type == "windows") {
    cores <- 1
  }

  one_run <- function(i) {
    set.seed(seed + i)
    d <- simulate_selection_data(n = n, p = p, beta = beta, rho = rho,
                                 sigma = sigma, family = family,
                                 intercept = intercept)
    fit <- compare_selection(y ~ ., data = d, methods = methods,
                             family = family, nfolds = nfolds)
    cbind(sim = i, score_selection(fit, attr(d, "truth")))
  }

  runs <- parallel::mclapply(seq_len(n_sim), one_run, mc.cores = cores)
  failed <- vapply(runs, inherits, logical(1), what = "try-error")
  if (any(failed)) {
    stop("A simulation failed: ", as.character(runs[[which(failed)[1]]]),
         call. = FALSE)
  }
  runs <- do.call(rbind, runs)
  rownames(runs) <- NULL
  runs$method <- factor(runs$method, levels = methods)

  by_method <- split(runs, runs$method)
  summary_table <- do.call(rbind, lapply(names(by_method), function(m) {
    r <- by_method[[m]]
    exact <- mean(r$exact)
    data.frame(
      method = m,
      sensitivity = mean(r$sensitivity),
      false_positive_rate = mean(r$false_positive_rate),
      exact = exact,
      mcse_exact = sqrt(exact * (1 - exact) / nrow(r)),
      n_selected = mean(r$n_selected),
      stringsAsFactors = FALSE
    )
  }))
  rownames(summary_table) <- NULL

  structure(
    list(summary = summary_table, runs = runs,
         settings = list(n = n, p = p, beta = beta, rho = rho, sigma = sigma,
                         family = family, n_sim = n_sim, seed = seed)),
    class = "selection_sim"
  )
}

#' @export
print.selection_sim <- function(x, ...) {
  s <- x$settings
  cat("Simulation: ", s$n_sim, " data sets, n = ", s$n, ", p = ", s$p, ", ",
      length(s$beta), " true variable(s), rho = ", s$rho, ", family: ",
      s$family, "\n\n", sep = "")
  shown <- x$summary
  shown[-1] <- lapply(shown[-1], round, 3)
  print(shown, row.names = FALSE)
  invisible(x)
}
