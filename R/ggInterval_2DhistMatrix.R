#' @name ggInterval_2DhistMatrix
#' @title 2-Dimension histogram matrix
#' @description Visualize all continuous interval-valued variables with a
#' matrix of 2D histograms. Each off-diagonal panel shows a 2D histogram for
#' a pair of variables, and each diagonal panel displays the variable name.
#' @import tidyverse rlang ggplot2
#' @importFrom RSDA is.sym.interval
#' @importFrom magrittr %>%
#' @param data A ggInterval object. It can also be either an RSDA object or
#' a classical data frame, which will be automatically converted to
#' ggInterval data.
#' @param mapping Set of aesthetic mappings created by aes() or aes_().
#' If specified and inherit.aes = TRUE (the default), it is combined with
#' the default mapping at the top level of the plot. It is the same as the
#' mapping of ggplot2. This function ignores \code{x} and \code{y} mappings
#' and plots all continuous interval-valued variables.
#' @param method Histogram partition method. Use \code{"equal-bin"} for
#' equally spaced bins or \code{"unequal-bin"} for non-equidistant bins
#' defined by the observed interval endpoints.
#' @param xBins Number of x-axis bins used when \code{method = "equal-bin"}.
#' @param yBins Number of y-axis bins used when \code{method = "equal-bin"}.
#' @param display Metric shown in the cells. Use \code{"p"} for relative
#' frequency, \code{"f"} for weighted frequency, or \code{"h"} for density.
#' @param palette ColorBrewer palette passed to \code{scale_fill_distiller()}.
#' @param direction Direction passed to \code{scale_fill_distiller()}.
#' @param tau Non-negative tolerance used when \code{method = "unequal-bin"}.
#' Consecutive breakpoints whose gaps are smaller than \code{tau} are merged.
#' @param removeZero Whether remove cells whose frequency is equal to zero.
#' @param cell_labels Logical. If \code{TRUE}, add cell labels.
#' @param label_rule Rule used when \code{cell_labels = TRUE}. The default
#' \code{"above-mean"} shows only cells whose displayed values exceed the
#' mean value within the matrix. Alternatives are \code{"nonzero"} and
#' \code{"all"}.
#' @param addFreq Deprecated alias for \code{cell_labels}.
#' @return Return a ggplot2 object.
#' @usage ggInterval_2DhistMatrix(data = NULL, mapping = aes(NULL),
#' method = "equal-bin", xBins = 8, yBins = 8,
#' display = "p", palette = "Blues", direction = 1, tau = 0,
#' removeZero = FALSE, cell_labels = FALSE, label_rule = "above-mean",
#' addFreq = NULL)
#'
#' @examples
#' ggInterval_2DhistMatrix(
#'   oils,
#'   xBins = 5,
#'   yBins = 5,
#'   display = "p",
#'   palette = "Blues",
#'   cell_labels = TRUE
#' )
#'
#' ggInterval_2DhistMatrix(
#'   oils,
#'   method = "unequal-bin",
#'   display = "p",
#'   palette = "Blues",
#'   tau = 0.5
#' )
#'
#' @export
ggInterval_2DhistMatrix <- function(data = NULL,
                                    mapping = aes(NULL),
                                    method = "equal-bin",
                                    xBins = 8,
                                    yBins = 8,
                                    display = "p",
                                    palette = "Blues",
                                    direction = 1,
                                    tau = 0,
                                    removeZero = FALSE,
                                    cell_labels = FALSE,
                                    label_rule = "above-mean",
                                    addFreq = NULL) {
  . <- NULL
  method <- match_hist2d_method(method)
  display <- match_hist2d_display(display)
  label_rule <- match.arg(label_rule, c("above-mean", "nonzero", "all"))
  if (!is.null(addFreq)) {
    cell_labels <- addFreq
  }

  if (method == "equal-bin" && xBins + yBins > 100) {
    stop("ERROR : Bins are too large to calculate. Suggest both axes use fewer than 50 bins.")
  }

  ggSymData <- testData(data)
  iData <- ggSymData$intervalData
  n <- nrow(iData)

  if (n > 50) {
    stop("Out of time limits. Suggested number of observations should be less than 50.")
  }

  numericData <- unlist(lapply(iData, FUN = RSDA::is.sym.interval))
  iData <- iData[, numericData, drop = FALSE]

  if (ncol(iData) < 2) {
    stop("At least two numeric interval-valued variables are required.")
  }

  if (ncol(iData) < dim(ggSymData$intervalData)[2]) {
    warning("Ignore non-numeric data.")
  }

  p <- ncol(iData)
  if (n * p > 200) {
    stop("Out of time limits. Suggested dimension should be less than (50 x 4).")
  }

  x_bin_sizes <- vapply(
    seq_len(p),
    FUN.VALUE = numeric(1),
    FUN = function(i) {
      length(build_hist2d_breaks(iData[[i]], xBins, method, tau = tau)) - 1
    }
  )
  y_bin_sizes <- vapply(
    seq_len(p),
    FUN.VALUE = numeric(1),
    FUN = function(i) {
      length(build_hist2d_breaks(iData[[i]], yBins, method, tau = tau)) - 1
    }
  )
  complexity <- n * p * p * max(x_bin_sizes) * max(y_bin_sizes)
  if (complexity >= 2000000 && complexity < 8000000) {
    warning("It is not recommended number of variables and bins be too large.")
  } else if (complexity >= 8000000) {
    stop("Out of time limits. The sample size, dimensions, and bins are too large.")
  }

  usermapping <- mapping[setdiff(names(mapping), c("x", "y"))]
  freq.matrix <- NULL
  for (i in seq_len(p)) {
    var_x <- colnames(iData)[i]
    x_breaks <- build_hist2d_breaks(iData[[i]], xBins, method, tau = tau)
    x_mid <- (min(x_breaks) + max(x_breaks)) / 2

    for (u in seq_len(p)) {
      var_y <- colnames(iData)[u]

      if (i == u) {
        diag_row <- data.frame(
          f = 0,
          p = 0,
          h = 0,
          x1 = min(x_breaks),
          x2 = max(x_breaks),
          y1 = min(x_breaks),
          y2 = max(x_breaks),
          area = (max(x_breaks) - min(x_breaks))^2,
          xmid = x_mid,
          ymid = x_mid,
          display_value = 0,
          display_metric = display,
          xv = var_x,
          yv = var_y,
          isPlot = FALSE,
          textXY = x_mid
        )
        freq.matrix <- rbind(
          freq.matrix,
          diag_row
        )
      } else {
        pair_data <- build_hist2d_data(
          iData = iData,
          attr1 = var_x,
          attr2 = var_y,
          xBins = xBins,
          yBins = yBins,
          method = method,
          tau = tau
        )$data
        pair_data <- hist2d_assign_display(pair_data, display = display)
        pair_data$xv <- var_x
        pair_data$yv <- var_y
        pair_data$isPlot <- TRUE
        pair_data$textXY <- NA_real_
        freq.matrix <- rbind(freq.matrix, pair_data)
      }
    }
  }

  if (removeZero) {
    freq.matrix <- rbind(
      freq.matrix[freq.matrix$display_value != 0 | !freq.matrix$isPlot, , drop = FALSE]
    )
  }

  plot_cells <- freq.matrix[freq.matrix$isPlot, , drop = FALSE]
  border_linewidth <- if (method == "unequal-bin") 0.15 else 0.3

  mymapping <- list(
    data = . %>% dplyr::filter(.data$isPlot),
    mapping = aes(
      xmin = .data$x1,
      xmax = .data$x2,
      ymin = .data$y1,
      ymax = .data$y2,
      fill = .data$display_value
    ),
    alpha = 0.5,
    linewidth = border_linewidth
  )
  allmapping <- as.list(structure(as.expression(c(
    usermapping, mymapping
  )), class = "uneval"))

  base <- ggplot(data = freq.matrix, aes(.data$x1, .data$y1)) +
    do.call(geom_rect, allmapping) +
    geom_text(
      data = . %>% dplyr::filter(!.data$isPlot),
      aes(
        x = .data$textXY,
        y = .data$textXY,
        label = .data$xv
      ),
      size = 12
    ) +
    facet_grid(.data$yv ~ .data$xv, scales = "free") +
    scale_fill_distiller(
      name = hist2d_legend_name(display),
      palette = palette,
      direction = direction,
      limits = c(0, max(plot_cells$display_value))
    ) +
    labs(x = "", y = "") +
    theme_bw()

  if (cell_labels) {
    label_data <- switch(
      label_rule,
      "above-mean" = plot_cells[plot_cells$display_value > mean(plot_cells$display_value), , drop = FALSE],
      "nonzero" = plot_cells[plot_cells$display_value != 0, , drop = FALSE],
      "all" = plot_cells
    )
    if (nrow(label_data) != 0) {
      label_data$cell_label <- format_hist2d_labels(label_data$display_value)
      base <- base + geom_text(
        data = label_data,
        aes(
          x = .data$xmid,
          y = .data$ymid,
          label = .data$cell_label
        ),
        inherit.aes = FALSE
      )
    }
  }

  base
}
