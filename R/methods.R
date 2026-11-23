# The individual selection methods. Each takes the output of
# prepare_selection() and returns the names of the variables it selects.
# None of these are exported.

# Stepwise selection with stats::step(). `direction` is "forward", "backward"
# or "both"; `k` is 2 for AIC and log(n) for BIC. Forward and bidirectional
# selection start from the model with no predictors, backward selection from
# the model with all of them.
select_stepwise <- function(prep, family, direction, k) {
  lhs <- as.character(prep$formula[[2]])
  null_formula <- stats::reformulate("1", response = lhs)
  fit_model <- function(f) {
    if (family == "gaussian") {
      stats::lm(f, data = prep$data)
    } else {
      stats::glm(f, family = stats::binomial(), data = prep$data)
    }
  }
  start <- if (direction == "backward") prep$formula else null_formula
  fit <- fit_model(start)
  scope <- list(lower = ~ 1, upper = prep$formula)
  fit <- suppressWarnings(
    stats::step(fit, scope = scope, direction = direction, k = k, trace = 0)
  )
  attr(stats::terms(fit), "term.labels")
}

# The lasso with the penalty chosen by cross-validation. `rule` is
# "lambda.min" (smallest cross-validated error) or "lambda.1se" (the largest
# penalty within one standard error of that).
select_lasso <- function(prep, family, rule, nfolds) {
  if (ncol(prep$x) < 2) {
    stop("The lasso needs at least two predictor columns.", call. = FALSE)
  }
  fit <- glmnet::cv.glmnet(prep$x, prep$y, family = family, alpha = 1,
                           nfolds = nfolds)
  beta <- as.matrix(stats::coef(fit, s = rule))[-1, 1]
  columns <- which(beta != 0)
  columns_to_terms(columns, prep)
}

# Best subset selection for linear models: for every number of columns, the
# subset with the smallest residual sum of squares, then the number chosen by
# AIC or BIC. A factor counts as selected if any of its dummy columns is.
select_best_subset <- function(prep, criterion) {
  n <- length(prep$y)
  p <- ncol(prep$x)
  if (p > 25) {
    stop("Best subset selection searches every combination and is limited ",
         "to 25 predictor columns; there are ", p, ".", call. = FALSE)
  }
  search <- leaps::regsubsets(prep$x, prep$y, nvmax = p, nbest = 1,
                              method = "exhaustive", intercept = TRUE)
  summary_search <- summary(search)
  rss <- c(sum((prep$y - mean(prep$y))^2), summary_search$rss)
  size <- 0:p
  penalty <- if (criterion == "aic") 2 else log(n)
  value <- n * log(rss / n) + penalty * (size + 1)
  best_size <- size[which.min(value)]
  if (best_size == 0) {
    return(character(0))
  }
  columns <- which(summary_search$which[best_size, -1])
  columns_to_terms(columns, prep)
}

# Run one named method.
run_method <- function(method, prep, family, nfolds) {
  switch(method,
    forward_aic  = select_stepwise(prep, family, "forward", 2),
    backward_aic = select_stepwise(prep, family, "backward", 2),
    both_aic     = select_stepwise(prep, family, "both", 2),
    forward_bic  = select_stepwise(prep, family, "forward", log(length(prep$y))),
    backward_bic = select_stepwise(prep, family, "backward", log(length(prep$y))),
    both_bic     = select_stepwise(prep, family, "both", log(length(prep$y))),
    lasso_min    = select_lasso(prep, family, "lambda.min", nfolds),
    lasso_1se    = select_lasso(prep, family, "lambda.1se", nfolds),
    best_aic     = select_best_subset(prep, "aic"),
    best_bic     = select_best_subset(prep, "bic")
  )
}
