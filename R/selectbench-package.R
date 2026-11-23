#' selectbench: Compare Variable Selection Methods and Score Them Against the Truth
#'
#' Different variable selection methods often pick different variables from
#' the same data. selectbench runs several of them side by side, shows what
#' each one picked, and, when the true variables are known, scores each
#' method on what it found and what it got wrong.
#'
#' @section Compare methods on your data:
#' * [compare_selection()] runs the methods and returns a table of which
#'   variable each method selected.
#' * [plot.selection_compare()] draws that table.
#'
#' @section Score methods against a known truth:
#' * [simulate_selection_data()] generates data with known true variables.
#' * [score_selection()] scores one comparison against the truth.
#' * [simulate_selection()] repeats this over many simulated data sets.
#' * [plot.selection_sim()] plots the result.
#'
#' @section Example data:
#' * [selection_demo]: simulated data in which only `x1`, `x2` and `x3` matter.
#'
#' @keywords internal
"_PACKAGE"
