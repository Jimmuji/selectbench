#' Plot the variables each method selected
#'
#' Draws one tile for every variable and method, filled when the method
#' selected the variable. Variables are ordered with the most widely selected
#' at the top, so the variables all methods agree on sit together and the
#' contested ones are easy to see.
#'
#' @param x A `selection_compare` object from [compare_selection()].
#' @param truth Optional character vector with the names of the true
#'   variables. If given, true variables are marked on the axis.
#' @param ... Not used.
#'
#' @return A `ggplot` object, which can be changed further with the usual
#'   ggplot2 functions.
#'
#' @examples
#' result <- compare_selection(y ~ ., data = selection_demo, seed = 1)
#' plot(result)
#' plot(result, truth = c("x1", "x2", "x3"))
#'
#' @importFrom ggplot2 .data
#' @export
plot.selection_compare <- function(x, truth = NULL, ...) {
  if (!is.null(truth)) {
    unknown <- setdiff(truth, x$variables)
    if (length(unknown) > 0) {
      stop("`truth` names a variable that is not in the data: '", unknown[1],
           "'.", call. = FALSE)
    }
  }

  # most widely selected first; ties keep the order of the formula
  ordered_vars <- x$variables[order(-x$n_methods[x$variables],
                                    match(x$variables, x$variables))]
  long <- data.frame(
    variable = rep(x$variables, times = length(x$methods)),
    method = rep(x$methods, each = length(x$variables)),
    selected = unlist(lapply(x$methods, function(m) x$table[[m]]))
  )
  long$variable <- factor(long$variable, levels = rev(ordered_vars))
  long$method <- factor(long$method, levels = x$methods)
  long$state <- factor(ifelse(long$selected, "Selected", "Not selected"),
                       levels = c("Selected", "Not selected"))

  y_labels <- levels(long$variable)
  if (!is.null(truth)) {
    y_labels <- ifelse(y_labels %in% truth, paste0(y_labels, " *"), y_labels)
  }

  plot <- ggplot2::ggplot(
    long, ggplot2::aes(x = .data$method, y = .data$variable,
                       fill = .data$state)
  ) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.8) +
    ggplot2::geom_point(
      data = function(d) d[d$selected, , drop = FALSE],
      shape = 16, size = 2.5, colour = "white"
    ) +
    ggplot2::scale_fill_manual(
      values = c("Selected" = "#2a78d6", "Not selected" = "#e6e5e1"),
      name = NULL
    ) +
    ggplot2::scale_x_discrete(expand = c(0, 0)) +
    ggplot2::scale_y_discrete(labels = y_labels, expand = c(0, 0)) +
    ggplot2::labs(
      x = NULL, y = NULL,
      title = "Variables selected by each method",
      caption = if (!is.null(truth)) "* true variable" else NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   legend.position = "top",
                   axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
  plot
}

#' Plot a simulation comparison of selection methods
#'
#' Plots each method as a point: the share of true variables it finds
#' (sensitivity, up) against the share of noise variables it picks up (false
#' positive rate, right). A method in the top left corner finds the true
#' variables without picking up noise.
#'
#' @param x A `selection_sim` object from [simulate_selection()].
#' @param ... Not used.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' sim <- simulate_selection(n = 100, p = 8, n_sim = 20, cores = 1)
#' plot(sim)
#'
#' @importFrom ggplot2 .data
#' @export
plot.selection_sim <- function(x, ...) {
  d <- x$summary
  d$family <- factor(method_family(d$method),
                     levels = c("Stepwise", "Lasso", "Best subset"))

  # the axes start at 0 and 1 is the best possible sensitivity; the other
  # ends are rounded to 0.1 so that all methods fit with some room
  x_max <- min(1, max(0.1, ceiling(max(d$false_positive_rate) * 12) / 10))
  y_min <- max(0, floor((min(d$sensitivity) - 0.05) * 10) / 10)

  ggplot2::ggplot(
    d, ggplot2::aes(x = .data$false_positive_rate, y = .data$sensitivity,
                    colour = .data$family)
  ) +
    ggplot2::geom_point(size = 3.5) +
    ggrepel::geom_text_repel(ggplot2::aes(label = .data$method), size = 3,
                             show.legend = FALSE, max.overlaps = Inf,
                             min.segment.length = 0.2, seed = 1) +
    ggplot2::scale_colour_manual(
      values = c("Stepwise" = "#2a78d6", "Lasso" = "#eb6834",
                 "Best subset" = "#1baf7a"),
      name = NULL, drop = FALSE
    ) +
    ggplot2::scale_x_continuous(limits = c(0, x_max)) +
    ggplot2::scale_y_continuous(limits = c(y_min, 1)) +
    ggplot2::labs(
      x = "False positive rate (noise variables selected)",
      y = "Sensitivity (true variables found)",
      title = "Selection methods compared over simulated data sets",
      subtitle = paste0(x$settings$n_sim, " data sets; top left is best")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}
