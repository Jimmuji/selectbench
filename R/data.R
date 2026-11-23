#' Simulated data with three true variables
#'
#' A data set for the examples. The outcome `y` depends on `x1`, `x2` and `x3`
#' only. The other seven predictors have no effect on `y`. All predictors are
#' moderately correlated with each other (correlation 0.3), which makes it
#' easier for a selection method to pick a noise variable.
#'
#' The model used to generate the data is
#' `y = 2 * x1 - 1.5 * x2 + 1 * x3 + error`, with error standard deviation 1.
#' The true variables are stored in `attr(selection_demo, "truth")`.
#'
#' @format A data frame with 200 rows and 11 columns:
#' \describe{
#'   \item{y}{Numeric outcome.}
#'   \item{x1, x2, x3}{The true variables.}
#'   \item{x4, x5, x6, x7, x8, x9, x10}{Noise variables with no effect.}
#' }
#' @source Simulated. The script is `data-raw/selection_demo.R` in the package
#'   repository.
"selection_demo"
