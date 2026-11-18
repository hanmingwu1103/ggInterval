#' Decompose endpoint deviation into center and range contributions
#'
#' Each bar decomposes an observation's squared endpoint deviation from a
#' reference into center and full-range contributions. By default the reference
#' consists of the componentwise sample medians of the centers and full ranges,
#' separately within each supplied group. Without groups, pooled medians are
#' used. This is a descriptive geometric decomposition, not a Shapley
#' attribution or a calibrated outlier test.
#'
#' @param data Interval input accepted by ggInterval: a list with numeric
#'   matrices named `lower` and `upper`, or a supported symbolic interval table.
#' @param variables Selected interval-variable names; `NULL` selects
#'   all interval variables.
#' @param group Optional grouping vector of length equal to the number of rows.
#' @param scale Positive finite numeric vector, one scale per selected variable.
#'   The default is each variable's global endpoint span. Variables whose
#'   default span is zero are omitted and reported in the diagnostics.
#'   If a supplied reference differs on a constant variable, explicit positive
#'   scales are required so its deviation is not silently omitted.
#' @param reference Optional list with finite numeric vectors `center` and
#'   `range`, each of length equal to the number of selected variables. This
#'   supplies one common reference for every observation, overriding the
#'   group-median references. Reference full ranges must be nonnegative.
#' @param labels Optional distinct, nonmissing character observation IDs.
#' @param highlight Optional logical vector identifying observations to outline.
#'   It must contain externally supplied labels, such as known injected rows;
#'   the function does not detect outliers.
#'
#' @details For center difference `dc`, full-range difference `dr` and positive
#'   scale `s`, the contributions are `sum((dc/s)^2)` and
#'   `sum((dr/(2*s))^2)`. Their sum equals the sum, over variables, of the mean
#'   squared standardized deviations of the two endpoints from reference
#'   endpoints `center - range/2` and `center + range/2`. Bars are ordered by
#'   decreasing total deviation, with input order breaking ties. All observations
#'   remain in the plot. A supplied reference need not be an observed interval.
#'
#' @return An ordinary `ggplot` object. Its `interval_diagnostics` attribute
#'   contains the contributions in input order, reference matrices, scales,
#'   omitted variables, plotting order, and the endpoint identity check.
#' @examples
#' library(ggInterval)
#' endpoints <- list(
#'   lower = cbind(X1 = c(0, 1, 4), X2 = c(1, 0, 3)),
#'   upper = cbind(X1 = c(2, 5, 6), X2 = c(3, 2, 7)))
#' ggInterval_deviation(endpoints)
#' ggInterval_deviation(endpoints, scale = c(2, 2),
#'   reference = list(center = c(2, 2), range = c(2, 2)))
#' @importFrom rlang .data
#' @export
ggInterval_deviation <- function(data, variables = NULL, group = NULL,
                                 scale = NULL, reference = NULL,
                                 labels = NULL, highlight = NULL) {
  z <- .ggInterval_endpoint_inputs(data, variables)
  n <- z$n
  p <- z$p
  if (is.null(group)) group <- rep("All observations", n)
  if (length(group) != n || anyNA(group)) {
    stop("`group` must contain one nonmissing value per observation.", call. = FALSE)
  }
  groups <- as.character(group)
  if (is.null(labels)) labels <- as.character(z$ids)
  if (!is.character(labels) || length(labels) != n || anyNA(labels) ||
      any(!nzchar(labels)) || anyDuplicated(labels)) {
    stop("`labels` must contain distinct, nonempty character IDs, one per observation.",
         call. = FALSE)
  }
  if (is.null(highlight)) highlight <- rep(FALSE, n)
  if (!is.logical(highlight) || length(highlight) != n || anyNA(highlight)) {
    stop("`highlight` must be a nonmissing logical vector, one value per observation.",
         call. = FALSE)
  }
  center <- z$lower / 2 + z$upper / 2
  ranges <- z$upper - z$lower
  if (any(!is.finite(ranges))) {
    stop("Endpoint differences must be finite; rescale the input first.", call. = FALSE)
  }
  default_scale <- is.null(scale)
  if (default_scale) {
    scale <- apply(z$upper, 2L, max) - apply(z$lower, 2L, min)
    if (any(!is.finite(scale))) {
      stop("Global endpoint spans must be finite; rescale the input first.", call. = FALSE)
    }
    keep <- scale > 0
  } else {
    if (!is.numeric(scale) || !is.null(dim(scale)) || length(scale) != p || any(!is.finite(scale)) ||
        any(scale <= 0)) {
      stop("`scale` must contain one positive finite number per selected variable.",
           call. = FALSE)
    }
    keep <- rep(TRUE, p)
  }
  names(scale) <- z$variables
  reference_center <- matrix(NA_real_, n, p, dimnames = dimnames(center))
  reference_range <- reference_center
  if (is.null(reference)) {
    for (g in unique(groups)) {
      rows <- which(groups == g)
      reference_center[rows, ] <- rep(
        apply(center[rows, , drop = FALSE], 2L, stats::median), each = length(rows))
      reference_range[rows, ] <- rep(
        apply(ranges[rows, , drop = FALSE], 2L, stats::median), each = length(rows))
    }
    reference_type <- if (length(unique(groups)) == 1L) "pooled medians" else "group medians"
  } else {
    valid_vector <- function(x) {
      is.numeric(x) && is.null(dim(x)) && length(x) == p && all(is.finite(x))
    }
    if (!is.list(reference) || !valid_vector(reference$center) ||
        !valid_vector(reference$range) || any(reference$range < 0)) {
      stop("`reference` must contain finite numeric `center` and nonnegative full `range` vectors, one value per selected variable.",
           call. = FALSE)
    }
    reference_center[,] <- rep(reference$center, each = n)
    reference_range[,] <- rep(reference$range, each = n)
    reference_type <- "supplied common reference"
  }
  if (any(!keep) &&
      (any(center[, !keep, drop = FALSE] != reference_center[, !keep, drop = FALSE]) ||
       any(ranges[, !keep, drop = FALSE] != reference_range[, !keep, drop = FALSE]))) {
    stop("The supplied reference differs on a constant variable; provide explicit positive `scale` values to include that deviation.",
         call. = FALSE)
  }
  dc <- center[, keep, drop = FALSE] - reference_center[, keep, drop = FALSE]
  dr <- ranges[, keep, drop = FALSE] - reference_range[, keep, drop = FALSE]
  dc_scaled <- sweep(dc, 2L, scale[keep], "/")
  dr_scaled <- sweep(dr, 2L, scale[keep], "/") / 2
  center_contribution <- rowSums(dc_scaled^2)
  range_contribution <- rowSums(dr_scaled^2)
  ref_lower <- reference_center[, keep, drop = FALSE] -
    reference_range[, keep, drop = FALSE] / 2
  ref_upper <- reference_center[, keep, drop = FALSE] +
    reference_range[, keep, drop = FALSE] / 2
  lower_difference <- sweep(z$lower[, keep, drop = FALSE] - ref_lower,
                            2L, scale[keep], "/")
  upper_difference <- sweep(z$upper[, keep, drop = FALSE] - ref_upper,
                            2L, scale[keep], "/")
  endpoint_deviation <- rowSums((lower_difference^2 + upper_difference^2) / 2)
  total <- center_contribution + range_contribution
  if (any(!is.finite(total)) || any(!is.finite(endpoint_deviation))) {
    stop("The standardized deviations exceed the numeric range; choose larger scales.",
         call. = FALSE)
  }
  ordering <- order(-total, seq_len(n))
  contributions <- data.frame(id = labels, group = groups,
                              center = center_contribution, range = range_contribution,
                              total = total, highlighted = highlight,
                              stringsAsFactors = FALSE)
  plotted <- data.frame(
    id = factor(rep(labels, 2L), levels = labels[ordering]),
    contribution = c(center_contribution, range_contribution),
    component = factor(rep(c("Center", "Range"), each = n),
                       levels = c("Center", "Range")),
    highlighted = rep(highlight, 2L))
  plot <- ggplot2::ggplot(plotted, ggplot2::aes(
    x = .data$id, y = .data$contribution, fill = .data$component)) +
    ggplot2::geom_col(ggplot2::aes(colour = .data$highlighted), width = 0.82,
                      linewidth = 0.3, position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::scale_fill_manual(values = c(Center = "#2166AC", Range = "#D6604D")) +
    ggplot2::scale_colour_manual(values = c(`FALSE` = NA, `TRUE` = "#1A1A1A"),
                                 guide = "none") +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::labs(x = "Observation (decreasing total deviation)",
                  y = "Standardized squared endpoint deviation", fill = "Contribution",
                  title = "Center and range contributions",
                  subtitle = paste("Reference:", reference_type),
                  caption = if (any(highlight)) "Outlines mark supplied labels; no outlier test is performed." else NULL) +
    .ggInterval_diagnostic_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5))
  attr(plot, "interval_diagnostics") <- list(
    contributions = contributions, center = center, range = ranges,
    reference = list(center = reference_center, range = reference_range,
                     type = reference_type),
    scale = scale, variables = z$variables, used_variables = z$variables[keep],
    omitted_variables = z$variables[!keep], order = ordering,
    endpoint_squared_deviation = endpoint_deviation,
    identity_error = total - endpoint_deviation,
    interpretation = "Descriptive endpoint-deviation decomposition; not Shapley attribution or a calibrated outlier test.")
  plot
}
