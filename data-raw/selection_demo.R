# Simulated data for the examples. Run from the package root after
# devtools::load_all() to rebuild data/selection_demo.rda.

devtools::load_all()
set.seed(2025)
selection_demo <- simulate_selection_data(n = 200, p = 10,
                                          beta = c(2, -1.5, 1),
                                          rho = 0.3, sigma = 1)
stopifnot(identical(attr(selection_demo, "truth"), c("x1", "x2", "x3")))
dir.create("data", showWarnings = FALSE)
save(selection_demo, file = "data/selection_demo.rda", compress = "bzip2")
