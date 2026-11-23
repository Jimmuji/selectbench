# A hand-built result, so that the expected scores are known exactly.
# Variables a to e; the truth is a and b.
fake <- structure(
  list(
    selected = list(m1 = c("a", "b"),        # exact
                    m2 = c("a", "c", "d"),   # 1 found, 1 missed, 2 false
                    m3 = character(0)),      # nothing selected
    methods = c("m1", "m2", "m3"),
    variables = c("a", "b", "c", "d", "e")
  ),
  class = "selection_compare"
)

test_that("scores are computed correctly", {
  s <- score_selection(fake, truth = c("a", "b"))
  expect_equal(s$method, c("m1", "m2", "m3"))
  expect_equal(s$n_selected, c(2, 3, 0))
  expect_equal(s$true_found, c(2, 1, 0))
  expect_equal(s$missed, c(0, 1, 2))
  expect_equal(s$false_selected, c(0, 2, 0))
  expect_equal(s$sensitivity, c(1, 0.5, 0))
  expect_equal(s$false_positive_rate, c(0, 2 / 3, 0))
  expect_equal(s$exact, c(TRUE, FALSE, FALSE))
})

test_that("false positive rate is NA when every variable is true", {
  s <- score_selection(fake, truth = c("a", "b", "c", "d", "e"))
  expect_true(all(is.na(s$false_positive_rate)))
})

test_that("scores add up on a real result", {
  res <- compare_selection(y ~ ., selection_demo, seed = 1)
  s <- score_selection(res, attr(selection_demo, "truth"))
  expect_equal(s$true_found + s$missed, rep(3, nrow(s)))
  expect_equal(s$false_selected, s$n_selected - s$true_found)
  expect_equal(s$method, default_methods)
})

test_that("bad input gives a readable error", {
  expect_error(score_selection(list(), "a"), "compare_selection")
  expect_error(score_selection(fake, character(0)), "variable names")
  expect_error(score_selection(fake, "zzz"), "not in the data")
})
