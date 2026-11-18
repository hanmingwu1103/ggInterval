#' Empirical CDF envelopes for interval observations
#'
#' Display the lower and upper empirical CDF envelopes determined by interval
#' endpoints, optionally with the empirical CDF of the interval midpoints.
#'
#' @param data An interval input accepted by ggInterval: a symbolic
#'   table, an object with an `intervalData` component, or a list containing
#'   numeric `lower` and `upper` matrices.
#' @param variables Optional character vector selecting variables, in display
#'   order. The default uses all interval variables.
#' @param midpoint Logical scalar. Include a dashed midpoint ECDF when `TRUE`.
#'
#' @details For intervals \eqn{[a_i,b_i]}, the lower envelope is
#'   \eqn{F_b(t)=n^{-1}\sum_i 1\{b_i\leq t\}} and the upper envelope is
#'   \eqn{F_a(t)=n^{-1}\sum_i 1\{a_i\leq t\}}. Thus \eqn{F_b(t)\leq F_a(t)}.
#'   Every empirical CDF formed by choosing one value from each interval lies
#'   between these envelopes. All observations receive equal weight. These are
#'   endpoint bounds, not a confidence band or a model for values inside an
#'   interval. The optional midpoint curve is a point-summary ECDF.
#'
#'   Bounds are evaluated at the sorted, unique endpoint knots. Shading uses
#'   constant-height rectangles between knots and boundary curves are
#'   right-continuous steps; no linear interpolation is used. Variables are
#'   faceted with separate horizontal scales and a common probability scale.
#'
#' @return An ordinary `ggplot` object. Its `interval_diagnostics` attribute is
#'   a list containing `knots` (exact endpoint CDF values), `band` (the constant
#'   segments used for shading, including plot margins), `midpoint` (exact
#'   midpoint knots and CDF values, or `NULL`), and `settings`. No rounded values
#'   are used in these calculations.
#' @export
#' @examples
#' library(ggInterval)
#' x <- list(lower = cbind(X = c(0, 2)), upper = cbind(X = c(2, 4)))
#' ggInterval_ecdf(x)
ggInterval_ecdf <- function(data, variables = NULL, midpoint = TRUE) {
  if (!is.logical(midpoint) || length(midpoint) != 1L || is.na(midpoint)) {
    stop("`midpoint` must be TRUE or FALSE.", call. = FALSE)
  }
  input <- .ggInterval_endpoint_inputs(data, variables)
  knot_tables <- band_tables <- bound_tables <- vector("list", input$p)
  midpoint_tables <- midpoint_plot_tables <- vector("list", input$p)

  for (j in seq_len(input$p)) {
    a <- input$lower[, j]
    b <- input$upper[, j]
    knots <- sort(unique(c(a, b)))
    lower_cdf <- findInterval(knots, sort(b)) / input$n
    upper_cdf <- findInterval(knots, sort(a)) / input$n
    knot_tables[[j]] <- data.frame(
      variable = input$variables[j], x = knots,
      lower_cdf = lower_cdf, upper_cdf = upper_cdf
    )

    span <- max(knots) - min(knots)
    padding <- if (span == 0) max(0.05 * abs(knots[1L]), 0.5) else {
      if (is.finite(span)) 0.04 * span else 0.04 * max(abs(knots))
    }
    plot_min <- max(-.Machine$double.xmax, min(knots) - padding)
    plot_max <- min(.Machine$double.xmax, max(knots) + padding)
    plot_x <- c(plot_min, knots, plot_max)
    plot_lower <- c(0, lower_cdf, 1)
    plot_upper <- c(0, upper_cdf, 1)
    bound_tables[[j]] <- data.frame(
      variable = input$variables[j], x = plot_x,
      lower_cdf = plot_lower, upper_cdf = plot_upper
    )
    band_tables[[j]] <- data.frame(
      variable = input$variables[j], xmin = plot_x[-length(plot_x)],
      xmax = plot_x[-1L],
      lower_cdf = plot_lower[-length(plot_lower)],
      upper_cdf = plot_upper[-length(plot_upper)]
    )
    if (midpoint) {
      centers <- a / 2 + b / 2
      center_knots <- sort(unique(centers))
      center_cdf <- findInterval(center_knots, sort(centers)) / input$n
      midpoint_tables[[j]] <- data.frame(
        variable = input$variables[j], x = center_knots, cdf = center_cdf
      )
      midpoint_plot_tables[[j]] <- data.frame(
        variable = input$variables[j],
        x = c(plot_min, center_knots, plot_max), cdf = c(0, center_cdf, 1)
      )
    }
  }
  knots <- do.call(rbind, knot_tables)
  band <- do.call(rbind, band_tables)
  bounds <- do.call(rbind, bound_tables)
  band$variable <- factor(band$variable, levels = input$variables)
  bounds$variable <- factor(bounds$variable, levels = input$variables)
  caption <- paste(
    "Endpoint envelopes: lower F_b(t), upper F_a(t).",
    "Shading bounds empirical CDF values; it is not a confidence band.",
    sep = "\n"
  )
  if (midpoint) caption <- paste(caption, "Dashed: midpoint ECDF.", sep = "\n")
  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = band,
      ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                   ymin = .data$lower_cdf, ymax = .data$upper_cdf),
      fill = "#6BAED6", alpha = 0.35, colour = NA
    ) +
    ggplot2::geom_step(
      data = bounds, ggplot2::aes(x = .data$x, y = .data$lower_cdf),
      direction = "hv", colour = "#08519C", linewidth = 0.6
    ) +
    ggplot2::geom_step(
      data = bounds, ggplot2::aes(x = .data$x, y = .data$upper_cdf),
      direction = "hv", colour = "#08519C", linewidth = 0.6
    )
  mid <- NULL
  if (midpoint) {
    mid <- do.call(rbind, midpoint_tables)
    mid_plot <- do.call(rbind, midpoint_plot_tables)
    mid_plot$variable <- factor(mid_plot$variable, levels = input$variables)
    p <- p + ggplot2::geom_step(
      data = mid_plot, ggplot2::aes(x = .data$x, y = .data$cdf),
      direction = "hv", colour = "#222222", linetype = "dashed",
      linewidth = 0.55
    )
  }
  p <- p + ggplot2::facet_wrap(~variable, scales = "free_x") +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
    ggplot2::labs(x = "Value", y = "Cumulative proportion", caption = caption) +
    .ggInterval_diagnostic_theme()
  attr(p, "interval_diagnostics") <- list(
    knots = knots, band = band, midpoint = mid,
    settings = list(variables = input$variables, n = input$n,
                    midpoint = midpoint, weighting = "equal observation weights",
                    boundary = "right-continuous; endpoints included")
  )
  p
}
