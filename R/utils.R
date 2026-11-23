# Internal helpers. None of these are exported.

# The methods the package knows about, in the order they are shown by default.
all_methods <- c("forward_aic", "backward_aic", "both_aic",
                 "forward_bic", "backward_bic", "both_bic",
                 "lasso_min", "lasso_1se", "best_aic", "best_bic")

default_methods <- c("forward_aic", "backward_aic", "both_bic",
                     "lasso_min", "lasso_1se", "best_bic")

# The methods run when the user does not choose: best subset only exists for
# linear models, so it is left out for binary outcomes.
resolve_methods <- function(methods, family) {
  if (is.null(methods)) {
    if (family == "binomial") {
      return(setdiff(default_methods, c("best_aic", "best_bic")))
    }
    return(default_methods)
  }
  methods
}

# Method family, used to colour the plots.
method_family <- function(methods) {
  ifelse(grepl("^lasso", methods), "Lasso",
         ifelse(grepl("^best", methods), "Best subset", "Stepwise"))
}

# Check the arguments shared by compare_selection() and simulate_selection().
check_methods <- function(methods, family) {
  if (!is.character(methods) || length(methods) == 0 || anyNA(methods)) {
    stop("`methods` must be one or more method names in quotes.", call. = FALSE)
  }
  unknown <- setdiff(methods, all_methods)
  if (length(unknown) > 0) {
    stop("Unknown method: '", paste(unknown, collapse = "', '"),
         "'. Choose from: ", paste(all_methods, collapse = ", "), ".",
         call. = FALSE)
  }
  if (anyDuplicated(methods) > 0) {
    stop("`methods` must not repeat a method.", call. = FALSE)
  }
  if (family == "binomial" && any(grepl("^best", methods))) {
    stop("Best subset selection is only available for family = \"gaussian\". ",
         "Remove 'best_aic' and 'best_bic' from `methods`.", call. = FALSE)
  }
  invisible(TRUE)
}

check_family <- function(family) {
  if (!is.character(family) || length(family) != 1 ||
      !family %in% c("gaussian", "binomial")) {
    stop("`family` must be \"gaussian\" or \"binomial\".", call. = FALSE)
  }
  invisible(TRUE)
}

# Turn a binary outcome (0/1, TRUE/FALSE or a two level factor) into 0/1.
as_binary <- function(y) {
  if (is.logical(y)) {
    return(as.numeric(y))
  }
  if (is.factor(y) || is.character(y)) {
    y <- factor(y)
    if (nlevels(y) != 2) {
      stop("With family = \"binomial\" the outcome must have exactly two ",
           "values.", call. = FALSE)
    }
    return(as.numeric(y == levels(y)[2]))
  }
  if (!all(y %in% c(0, 1))) {
    stop("With family = \"binomial\" the outcome must be 0/1, TRUE/FALSE or ",
         "a two level factor.", call. = FALSE)
  }
  as.numeric(y)
}

# Prepare the data once for every method.
#   data    the rows with no missing value in any variable that is used
#   x       numeric matrix of predictors (factors expanded to dummy columns)
#   y       the outcome, numeric (0/1 for binomial)
#   assign  for each column of x, the index of the variable it belongs to
#   terms   the names of the variables, in formula order
prepare_selection <- function(formula, data, family) {
  if (!inherits(formula, "formula")) {
    stop("`formula` must be a formula such as y ~ .", call. = FALSE)
  }
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  check_family(family)

  tt <- stats::terms(formula, data = data)
  if (length(attr(tt, "term.labels")) == 0) {
    stop("`formula` has no predictors to select from.", call. = FALSE)
  }
  if (any(attr(tt, "order") > 1)) {
    stop("Interactions are not supported. Use only main effects.",
         call. = FALSE)
  }
  used <- all.vars(stats::formula(tt))
  absent <- setdiff(used, names(data))
  if (length(absent) > 0) {
    stop("`data` has no column called '", absent[1], "'.", call. = FALSE)
  }

  if (!is.name(stats::formula(tt)[[2]])) {
    stop("The outcome must be a single column name, not an expression.",
         call. = FALSE)
  }

  complete <- stats::complete.cases(data[, used, drop = FALSE])
  d <- data[complete, , drop = FALSE]
  rownames(d) <- NULL
  if (nrow(d) < 10) {
    stop("Fewer than 10 rows have no missing values.", call. = FALSE)
  }

  mf <- stats::model.frame(formula, d)
  y <- stats::model.response(mf)
  if (family == "gaussian") {
    if (!is.numeric(y)) {
      stop("With family = \"gaussian\" the outcome must be numeric.",
           call. = FALSE)
    }
  } else {
    y <- as_binary(y)
    d[[as.character(stats::formula(tt)[[2]])]] <- y
  }

  mm <- stats::model.matrix(tt, mf)
  keep <- attr(mm, "assign") > 0
  x <- mm[, keep, drop = FALSE]
  assign <- attr(mm, "assign")[keep]

  list(data = d, x = x, y = as.numeric(y), assign = assign,
       terms = attr(tt, "term.labels"), formula = stats::formula(tt),
       n_dropped = nrow(data) - nrow(d))
}

# Names of the variables that a set of selected columns of x belongs to. A
# variable counts as selected if any of its columns is selected.
columns_to_terms <- function(columns, prep) {
  prep$terms[sort(unique(prep$assign[columns]))]
}
