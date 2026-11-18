#' Display pairwise geometric overlap of interval boxes
#'
#' This display compares observations, not variables. Each observation is the
#' Cartesian product of its selected intervals. Colours show intersection volume
#' divided by union volume (box Jaccard overlap), not symbolic correlation or
#' agreement between unobserved within-box distributions.
#'
#' @param data Interval input accepted by ggInterval: a list with numeric
#'   matrices named `lower` and `upper`, or a supported symbolic interval table.
#' @param variables Selected interval-variable names; `NULL` selects
#'   all interval variables.
#' @param group Optional grouping vector. Factor levels determine group order;
#'   otherwise groups follow first appearance. Input order is retained within
#'   groups. Thin lines separate groups.
#' @param labels Optional distinct, nonmissing character observation IDs.
#' @param zero_width How to handle boxes with zero width on any selected axis.
#'   The default `"error"` explains that such boxes have zero volume. With
#'   `"na"`, every pair involving such a box is explicitly missing, including
#'   its diagonal, rather than being assigned an artificial zero or one.
#'
#' @details Volumes are evaluated through logarithms and relative scaling to
#'   avoid underflow from products across many axes. Disjoint positive-volume
#'   boxes have overlap zero, identical boxes have overlap one, and touching
#'   boundaries have zero intersection volume. Positive affine rescaling of any
#'   axis leaves the result unchanged. Axis labels are thinned for larger tables;
#'   diagnostics retain every observation ID and every pairwise value.
#'
#' @return An ordinary `ggplot` object. Its `interval_diagnostics` attribute
#'   contains the full overlap matrix in input order, observation IDs, group
#'   ordering, and the indices of degenerate boxes.
#' @examples
#' library(ggInterval)
#' endpoints <- list(
#'   lower = cbind(X1 = c(0, 1, 4), X2 = c(0, 1, 4)),
#'   upper = cbind(X1 = c(2, 3, 5), X2 = c(2, 3, 5)))
#' ggInterval_overlap(endpoints, labels = c("A", "B", "C"))
#' attr(ggInterval_overlap(endpoints), "interval_diagnostics")$overlap
#' @importFrom rlang .data
#' @export
ggInterval_overlap <- function(data, variables = NULL, group = NULL,
                               labels = NULL, zero_width = c("error", "na")) {
  zero_width <- match.arg(zero_width)
  z <- .ggInterval_endpoint_inputs(data, variables)
  n <- z$n
  if (is.null(group)) group <- rep("All observations", n)
  if (length(group) != n || anyNA(group)) {
    stop("`group` must contain one nonmissing value per observation.", call. = FALSE)
  }
  group_levels <- if (is.factor(group)) {
    levels(droplevels(group))
  } else {
    unique(as.character(group))
  }
  groups <- as.character(group)
  if (is.null(labels)) labels <- as.character(z$ids)
  if (!is.character(labels) || length(labels) != n || anyNA(labels) ||
      any(!nzchar(labels)) || anyDuplicated(labels)) {
    stop("`labels` must contain distinct, nonempty character IDs, one per observation.",
         call. = FALSE)
  }
  widths <- z$upper - z$lower
  if (any(!is.finite(widths))) {
    stop("Endpoint differences must be finite; rescale the input first.", call. = FALSE)
  }
  degenerate <- rowSums(widths == 0) > 0
  if (any(degenerate) && zero_width == "error") {
    stop(paste0("Zero-width intervals produce zero-volume boxes (observation IDs: ",
                paste(labels[degenerate], collapse = ", "),
                "). Select nondegenerate axes or use `zero_width = 'na'` to mark these pairs missing."),
         call. = FALSE)
  }
  log_volume <- rowSums(log(widths))
  overlap <- matrix(NA_real_, n, n, dimnames = list(labels, labels))
  valid <- which(!degenerate)
  for (j in valid) {
    for (i in valid[valid <= j]) {
      intersection_width <- pmax(0, pmin(z$upper[i, ], z$upper[j, ]) -
                                    pmax(z$lower[i, ], z$lower[j, ]))
      if (any(intersection_width == 0)) {
        value <- 0
      } else {
        log_intersection <- sum(log(intersection_width))
        common <- max(log_volume[i], log_volume[j])
        scaled_intersection <- exp(log_intersection - common)
        scaled_union <- exp(log_volume[i] - common) +
          exp(log_volume[j] - common) - scaled_intersection
        value <- min(1, max(0, scaled_intersection / scaled_union))
      }
      overlap[i, j] <- value
      overlap[j, i] <- value
    }
  }
  ordering <- order(match(groups, group_levels), seq_len(n))
  display_ids <- labels[ordering]
  displayed <- overlap[ordering, ordering, drop = FALSE]
  plotted <- expand.grid(x = seq_len(n), y = seq_len(n))
  plotted$overlap <- as.vector(displayed)
  # Matrix rows run from top to bottom, matching a conventional heatmap.
  plotted$y <- n + 1L - plotted$y
  boundary <- if (n > 1L) which(diff(match(groups[ordering], group_levels)) != 0) else integer()
  tick_index <- if (n <= 18L) seq_len(n) else unique(c(1L, round(seq(1, n, length.out = 12L)), n))
  plot <- ggplot2::ggplot(plotted, ggplot2::aes(x = .data$x, y = .data$y, fill = .data$overlap)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient(low = "#F7FBFF", high = "#08519C", limits = c(0, 1),
                                 na.value = "#BDBDBD", name = "Box overlap") +
    ggplot2::scale_x_continuous(breaks = tick_index, labels = display_ids[tick_index],
                                expand = c(0, 0), limits = c(0.5, n + 0.5)) +
    ggplot2::scale_y_continuous(breaks = n + 1L - tick_index,
                                labels = display_ids[tick_index], expand = c(0, 0),
                                limits = c(0.5, n + 0.5)) +
    ggplot2::coord_fixed() +
    ggplot2::labs(x = "Observation", y = "Observation", title = "Geometric overlap of interval boxes",
                  subtitle = "Intersection volume / union volume",
                  caption = if (any(degenerate)) "Grey cells involve a zero-volume box and are undefined." else NULL) +
    .ggInterval_diagnostic_theme() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                    axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5))
  if (length(boundary)) {
    plot <- plot + ggplot2::geom_vline(xintercept = boundary + 0.5, colour = "#737373", linewidth = 0.3) +
      ggplot2::geom_hline(yintercept = n + 0.5 - boundary, colour = "#737373", linewidth = 0.3)
  }
  attr(plot, "interval_diagnostics") <- list(
    overlap = overlap, ids = labels, variables = z$variables,
    group = groups, group_levels = group_levels, order = ordering,
    displayed_ids = display_ids, group_boundaries = boundary,
    degenerate = which(degenerate), zero_width = zero_width,
    interpretation = "Geometric Jaccard overlap between observation boxes; not variable correlation or within-box distributional agreement.")
  plot
}
