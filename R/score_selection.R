#' Score selection methods against the true variables
#'
#' When the variables that truly matter are known, as in simulated data, each
#' method can be scored on what it found and what it got wrong.
#'
#' @param x A `selection_compare` object from [compare_selection()].
#' @param truth Character vector with the names of the variables that truly
#'   matter.
#'
#' @return A data frame with one row per method and these columns:
#' \describe{
#'   \item{method}{The method.}
#'   \item{n_selected}{Number of variables selected.}
#'   \item{true_found}{True variables that were selected.}
#'   \item{missed}{True variables that were not selected.}
#'   \item{false_selected}{Noise variables that were selected.}
#'   \item{sensitivity}{`true_found` divided by the number of true variables.}
#'   \item{false_positive_rate}{`false_selected` divided by the number of
#'     noise variables. `NA` if every variable is a true variable.}
#'   \item{exact}{`TRUE` if the selected set equals the true set.}
#' }
#'
#' @examples
#' result <- compare_selection(y ~ ., data = selection_demo, seed = 1)
#' score_selection(result, truth = c("x1", "x2", "x3"))
#'
#' @export
score_selection <- function(x, truth) {
  if (!inherits(x, "selection_compare")) {
    stop("`x` must be the result of compare_selection().", call. = FALSE)
  }
  if (!is.character(truth) || length(truth) == 0 || anyNA(truth)) {
    stop("`truth` must be one or more variable names in quotes.",
         call. = FALSE)
  }
  unknown <- setdiff(truth, x$variables)
  if (length(unknown) > 0) {
    stop("`truth` names a variable that is not in the data: '", unknown[1],
         "'.", call. = FALSE)
  }
  truth <- unique(truth)
  n_noise <- length(x$variables) - length(truth)

  rows <- lapply(x$methods, function(m) {
    picked <- x$selected[[m]]
    found <- length(intersect(picked, truth))
    false_sel <- length(setdiff(picked, truth))
    data.frame(
      method = m,
      n_selected = length(picked),
      true_found = found,
      missed = length(truth) - found,
      false_selected = false_sel,
      sensitivity = found / length(truth),
      false_positive_rate = if (n_noise > 0) false_sel / n_noise else NA_real_,
      exact = setequal(picked, truth),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
