#' Compare which variables different selection methods pick
#'
#' Runs several variable selection methods on the same data and reports, for
#' every variable, which methods selected it. Methods often disagree, and
#' this makes the disagreement visible in one table.
#'
#' @section Methods:
#' Each method name has the form `<method>_<rule>`.
#' \describe{
#'   \item{`forward_aic`, `forward_bic`}{Forward stepwise selection, starting
#'     from no predictors, with [stats::step()].}
#'   \item{`backward_aic`, `backward_bic`}{Backward stepwise selection,
#'     starting from all predictors.}
#'   \item{`both_aic`, `both_bic`}{Bidirectional stepwise selection, starting
#'     from no predictors.}
#'   \item{`lasso_min`}{The lasso, with the penalty that gives the smallest
#'     cross-validated error ([glmnet::cv.glmnet()]).}
#'   \item{`lasso_1se`}{The lasso, with the largest penalty whose error is
#'     within one standard error of the smallest.}
#'   \item{`best_aic`, `best_bic`}{Best subset selection: the best subset of
#'     every size is found by exhaustive search ([leaps::regsubsets()]) and
#'     the size is chosen by AIC or BIC. Linear models only, and at most 25
#'     predictor columns.}
#' }
#' AIC uses a penalty of 2 per parameter and BIC a penalty of `log(n)`, so
#' BIC tends to select fewer variables.
#'
#' @section Categorical predictors:
#' A factor is expanded into dummy columns. A factor counts as selected if
#' any of its dummy columns is selected, so every row of the result is one
#' variable from the formula.
#'
#' @param formula A model formula with the outcome on the left, for example
#'   `y ~ .`. Only main effects are supported, and the outcome must be a
#'   single column.
#' @param data A data frame. Rows with a missing value in any variable that
#'   is used are dropped before any method is run, so that every method sees
#'   the same data.
#' @param methods Which methods to run, from the list above. By default six
#'   run: `"forward_aic"`, `"backward_aic"`, `"both_bic"`, `"lasso_min"`,
#'   `"lasso_1se"` and `"best_bic"`. For a binary outcome best subset is not
#'   available, so the default is the first five.
#' @param family `"gaussian"` for a numeric outcome or `"binomial"` for a
#'   binary outcome.
#' @param nfolds Number of cross-validation folds used by the lasso.
#' @param seed Optional seed. The lasso chooses its penalty by random
#'   cross-validation folds, so set this to get the same result every time.
#'
#' @return An object of class `selection_compare`, a list with these
#'   elements:
#' \describe{
#'   \item{table}{Data frame with one row per variable and one logical
#'     column per method, `TRUE` where the method selected the variable.}
#'   \item{selected}{A list with the names of the variables each method
#'     selected.}
#'   \item{n_selected}{Number of variables each method selected.}
#'   \item{n_methods}{For each variable, the number of methods that selected
#'     it.}
#'   \item{n_dropped}{Number of rows dropped for missing values.}
#' }
#' Use [plot()] on the result to draw the table, and [score_selection()] to
#' compare it with a known truth.
#'
#' @examples
#' # simulated data in which only x1, x2 and x3 matter
#' result <- compare_selection(y ~ ., data = selection_demo, seed = 1)
#' result
#'
#' # the table behind the printout
#' result$table
#'
#' # a real data set
#' compare_selection(mpg ~ ., data = mtcars, seed = 1)
#'
#' @export
compare_selection <- function(formula, data, methods = NULL,
                              family = "gaussian", nfolds = 10, seed = NULL) {
  check_family(family)
  methods <- resolve_methods(methods, family)
  check_methods(methods, family)
  if (!is.numeric(nfolds) || length(nfolds) != 1 || nfolds < 3) {
    stop("`nfolds` must be a single number of at least 3.", call. = FALSE)
  }
  prep <- prepare_selection(formula, data, family)
  if (!is.null(seed)) {
    set.seed(seed)
  }

  selected <- lapply(methods, run_method, prep = prep, family = family,
                     nfolds = nfolds)
  names(selected) <- methods

  tab <- data.frame(variable = prep$terms, stringsAsFactors = FALSE)
  for (m in methods) {
    tab[[m]] <- prep$terms %in% selected[[m]]
  }

  structure(
    list(
      table = tab,
      selected = selected,
      n_selected = vapply(selected, length, integer(1)),
      n_methods = stats::setNames(rowSums(tab[, methods, drop = FALSE]),
                                  tab$variable),
      methods = methods,
      family = family,
      n = nrow(prep$data),
      n_dropped = prep$n_dropped,
      variables = prep$terms
    ),
    class = "selection_compare"
  )
}

#' @export
print.selection_compare <- function(x, ...) {
  cat("Variable selection on ", x$n, " rows, ", length(x$variables),
      " variables (family: ", x$family, ")\n", sep = "")
  if (x$n_dropped > 0) {
    cat(x$n_dropped, " row(s) with missing values were dropped.\n", sep = "")
  }
  cat("\n")
  shown <- x$table
  shown[x$methods] <- lapply(shown[x$methods], function(v) ifelse(v, "x", "."))
  shown$n_methods <- as.integer(x$n_methods)
  shown <- shown[order(-shown$n_methods, match(shown$variable, x$variables)), ]
  rownames(shown) <- NULL
  print(shown, row.names = FALSE, right = FALSE)
  cat("\nVariables selected: ",
      paste0(names(x$n_selected), " = ", x$n_selected, collapse = ", "),
      "\n", sep = "")
  invisible(x)
}
