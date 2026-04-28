#' @name ggInterval_2Dhist
#' @title Visualize a 2-dimension histogram for interval-valued data
#' @description Visualize the joint distribution of two continuous
#' interval-valued variables by dividing the x axis and y axis into
#' rectangles and calculating the frequency of each observation interval
#' in every rectangle.
#' @import tidyverse rlang ggplot2
#' @importFrom RSDA is.sym.interval
#' @param data A ggInterval object. It can also be either an RSDA object or
#' a classical data frame, which will be automatically converted to
#' ggInterval data.
#' @param mapping Set of aesthetic mappings created by aes() or aes_().
#' If specified and inherit.aes = TRUE (the default), it is combined with
#' the default mapping at the top level of the plot. You must supply
#' mapping if there is no plot mapping. It is the same as the mapping of
#' ggplot2.
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
#' @param removeZero Whether remove data whose frequency is equal to zero.
#' @param cell_labels Logical. If \code{TRUE}, add frequency labels to cells.
#' @param label_rule Rule used when \code{cell_labels = TRUE}. The default
#' \code{"above-mean"} shows only cells whose frequencies exceed the mean
#' cell frequency. Alternatives are \code{"nonzero"} and \code{"all"}.
#' @param addFreq Deprecated alias for \code{cell_labels}.
#' @return Return a list containing a ggplot2 object and the corresponding
#' frequency table.
#' @usage ggInterval_2Dhist(data = NULL, mapping = aes(NULL),
#' method = "equal-bin", xBins = 14, yBins = 16,
#' display = "p", palette = "Blues", direction = 1, tau = 0,
#' removeZero = FALSE,
#' cell_labels = FALSE, label_rule = "above-mean", addFreq = NULL)
#' @examples
#' ggInterval_2Dhist(oils, aes(x = GRA, y = FRE),
#'   xBins = 5, yBins = 5,
#'   display = "p",
#'   palette = "Blues",
#'   cell_labels = TRUE)
#'
#' ggInterval_2Dhist(oils, aes(x = GRA, y = FRE),
#'   method = "unequal-bin",
#'   display = "p",
#'   palette = "Blues")
#'
#' @export
ggInterval_2Dhist <- function(data = NULL,
                              mapping = aes(NULL),
                              method = "equal-bin",
                              xBins = 14,
                              yBins = 16,
                              display = "p",
                              palette = "Blues",
                              direction = 1,
                              tau = 0,
                              removeZero = FALSE,
                              cell_labels = FALSE,
                              label_rule = "above-mean",
                              addFreq = NULL) {
  method <- match_hist2d_method(method)
  display <- match_hist2d_display(display)
  label_rule <- match.arg(label_rule, c("above-mean", "nonzero", "all"))
  if (!is.null(addFreq)) {
    cell_labels <- addFreq
  }

  if (method == "equal-bin" && xBins + yBins > 200) {
    stop("ERROR : Bins are too large to calculate. Suggest both axes use fewer than 100 bins.")
  }

  args_num <- length(mapping)
  args <- lapply(mapping[1:args_num], FUN = rlang::get_expr)
  this.x <- args$x
  this.y <- args$y

  ggSymData <- testData(data)
  iData <- ggSymData$intervalData
  plot_data <- if ("ggInterval" %in% class(data)) iData else data
  p <- dim(plot_data)[2]
  n <- dim(iData)[1]
  resultSet <- NULL

  with(plot_data, {
    attr1 <- which(unlist(lapply(
      plot_data[, 1:p], FUN = identical, x = eval(this.x)
    )))
    attr1 <- names(attr1)
    attr2 <- which(unlist(lapply(
      plot_data[, 1:p], FUN = identical, x = eval(this.y)
    )))
    attr2 <- names(attr2)

    if (length(attr1) == 0 || length(attr2) == 0) {
      stop("ERROR : Missing attributes x or y in data frame.")
    }

    if (all((!is.numeric(plot_data[[attr1]])), !RSDA::is.sym.interval(plot_data[[attr1]])) ||
      all((!is.numeric(plot_data[[attr2]])), !RSDA::is.sym.interval(plot_data[[attr2]]))) {
      stop("ERROR : Variables in 2D histogram can only be numeric.")
    }

    hist2d_data <- build_hist2d_data(
      iData = iData,
      attr1 = attr1,
      attr2 = attr2,
      xBins = xBins,
      yBins = yBins,
      method = method,
      tau = tau
    )

    work_size <- n * hist2d_data$xBins * hist2d_data$yBins
    if (work_size >= 250000 && work_size < 1000000) {
      warning("The number of observations and bins is not suggested be too large.")
    } else if (work_size >= 1000000) {
      stop("The number of observations and bins are too large and will exceed the time limit.")
    }

    freq.matrix <- hist2d_assign_display(hist2d_data$data, display = display)
    table_data <- hist2d_data

    if (removeZero) {
      freq.matrix <- freq.matrix[freq.matrix$display_value != 0, , drop = FALSE]
    }
    if (nrow(freq.matrix) == 0) {
      stop("All histogram cells have zero frequency after removeZero = TRUE.")
    }

    border_linewidth <- if (method == "unequal-bin") 0.15 else 0.3
    usermapping <- mapping[setdiff(names(mapping), c("x", "y"))]
    mymapping <- list(
      data = freq.matrix,
      mapping = aes(
        xmin = x1,
        xmax = x2,
        ymin = y1,
        ymax = y2,
        fill = display_value,
        alpha = 0.5
      ),
      linewidth = border_linewidth
    )
    allmapping <- as.list(structure(as.expression(c(
      usermapping, mymapping
    )), class = "uneval"))

    base <- ggplot(freq.matrix, aes(x1, y1)) +
      do.call(geom_rect, allmapping) +
      scale_fill_distiller(
        palette = palette,
        direction = direction,
        limits = c(0, max(freq.matrix$display_value)),
        name = hist2d_legend_name(display)
      ) +
      labs(
        x = attr1,
        y = attr2,
        title = if (method == "equal-bin") "2D hist." else "2D hist. (Non-equidistant-bin)"
      ) +
      guides(alpha = "none")

    if (cell_labels) {
      label_data <- switch(
        label_rule,
        "above-mean" = freq.matrix[freq.matrix$display_value > mean(freq.matrix$display_value), , drop = FALSE],
        "nonzero" = freq.matrix[freq.matrix$display_value != 0, , drop = FALSE],
        "all" = freq.matrix
      )
      if (nrow(label_data) != 0) {
        label_data$cell_label <- format_hist2d_labels(label_data$display_value)
        base <- base + geom_text(
          data = label_data,
          aes(
            x = xmid,
            y = ymid,
            label = cell_label
          ),
          inherit.aes = FALSE
        )
      }
    }

    resultSet[["plot"]] <- base
    resultSet[[paste0("Table (", attr1, ", ", attr2, ")")]] <- build_hist2d_table(
      hist2d_data = table_data,
      attr1 = attr1,
      attr2 = attr2,
      display = display
    )
    return(resultSet)
  })
}
