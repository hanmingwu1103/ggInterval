#' @name ggInterval_lineplot
#' @aliases ggInterval_tsplot
#' @title Interval-valued line plot
#' @description Visualize interval-valued data along an ordered horizontal
#' axis. A line can connect the centers of intervals at each position, with
#' crossbars or errorbars indicating the interval range. When the horizontal
#' axis is time, the display becomes an interval-valued time-series plot.
#' @import rlang ggplot2
#' @importFrom RSDA is.sym.interval
#' @param data A ggInterval object. It can also be either RSDA object or
#' classical data frame, which will be automatically convert to ggInterval
#' data.
#' @param mapping Set of aesthetic mappings created by aes() or aes_().
#' Must include x for the ordered variable and y for the interval variable.
#' Optional aesthetics include group and fill for multiple line series.
#' It is the same as the mapping of ggplot2.
#' @param barWidth The width of the crossbar or errorbar indicating
#' the interval range at each x position. Default is 0.5.
#' @param add_line Logical; if TRUE (default), connect interval centers with a
#' line. Set FALSE when adding a customized line layer with
#' \code{geom_line(...)}.
#' @return Return a ggplot2 object.
#' @usage ggInterval_lineplot(data = NULL, mapping = aes(NULL),
#' barWidth = 0.5, add_line = TRUE)
#' @usage ggInterval_tsplot(data = NULL, mapping = aes(NULL),
#' barWidth = 0.5, add_line = TRUE)
#' @examples
#' \donttest{
#' if (requireNamespace("TTR", quietly = TRUE)) {
#'   data("ttrc", package = "TTR")
#'   stock.data <- subset(
#'     ttrc[, c("Date", "Close", "Low", "High")],
#'     format(Date, "%Y-%m") %in% c("1985-01", "1985-02", "1985-03")
#'   )
#'   stock.data$Month <- factor(
#'     month.abb[as.integer(format(stock.data$Date, "%m"))],
#'     levels = month.abb[1:3]
#'   )
#'   stock.data$Day <- as.integer(format(stock.data$Date, "%d"))
#'   stock.i <- classic2sym(
#'     stock.data,
#'     groupby = "customize",
#'     minData = stock.data$Low,
#'     maxData = stock.data$High
#'   )
#'   ggInterval_lineplot(stock.i, aes(y = V1, x = Day), barWidth = 0.6) +
#'     geom_point(aes(y = Close), shape = 21, fill = "#D95F02",
#'                color = "black", size = 1.6, stroke = 0.2) +
#'     coord_cartesian(xlim = c(1, 31), expand = FALSE) +
#'     facet_wrap(~Month, ncol = 1, scales = "free_y") +
#'     scale_x_continuous(breaks = c(1, 8, 15, 22, 29)) +
#'     labs(x = "Day of month", y = "Price") +
#'     ggthemes::theme_economist() +
#'     theme(strip.text = element_text(face = "bold"))
#' }
#' }
#' @export
ggInterval_lineplot <- function(data = NULL,
                                mapping = aes(NULL),
                                barWidth = 0.5,
                                add_line = TRUE) {
  args_num <- length(mapping)
  args <- lapply(mapping[1:args_num], FUN = rlang::get_expr)
  this.x <- args$x
  this.y <- args$y
  this.fill <- args$fill
  this.group <- args$group

  ggSymData <- testData(data)
  iData <- ggSymData$intervalData
  plotData <- .prepare_lineplot_data(ggSymData)

  num_vars <- ncol(iData)
  n <- nrow(iData)

  if (is.null(this.x) || is.null(this.y)) {
    stop("ERROR : Line plot requires both x (ordered variable) and y (interval) in aes().")
  }

  x_value <- rlang::eval_tidy(this.x, data = plotData)
  if (!(length(x_value) %in% c(1, n))) {
    stop(paste0("Length of x must be equal to data (", n, ") or 1."))
  }
  if (length(x_value) == 1) {
    x_value <- rep(x_value, n)
  }

  y_value <- rlang::eval_tidy(this.y, data = plotData)
  attr <- NULL
  match_y <- unlist(lapply(
    as.data.frame(iData[, 1:num_vars, drop = FALSE]),
    FUN = identical,
    x = y_value
  ))
  if (any(match_y)) {
    attr <- names(match_y)[which(match_y)[1]]
  } else {
    stop("ERROR : Cannot find interval variable y in data.")
  }

  if (all(!is.numeric(iData[[attr]]), !RSDA::is.sym.interval(iData[[attr]]))) {
    stop("ERROR : Variable y in line plot must be numeric or interval type.")
  }

  plotData$.lineplot_x <- x_value
  plotData$.lineplot_ymin <- iData[[attr]]$min
  plotData$.lineplot_ymax <- iData[[attr]]$max
  plotData$.lineplot_mid <- (plotData$.lineplot_ymin + plotData$.lineplot_ymax) / 2

  if (!is.null(this.group)) {
    group_value <- rlang::eval_tidy(this.group, data = plotData)
    if (!(length(group_value) %in% c(1, n))) {
      stop(paste0("Length of group must be equal to data (", n, ") or 1."))
    }
    if (length(group_value) == 1) {
      group_value <- rep(group_value, n)
    }
    plotData$.lineplot_group <- as.factor(group_value)
  }

  if (!is.null(this.fill)) {
    fill_value <- rlang::eval_tidy(this.fill, data = plotData)
    if (!(length(fill_value) %in% c(1, n))) {
      stop(paste0("Length of fill must be equal to data (", n, ") or 1."))
    }
    if (length(fill_value) == 1) {
      fill_value <- rep(fill_value, n)
    }
    plotData$.lineplot_fill <- fill_value
  } else if (!is.null(this.group)) {
    plotData$.lineplot_fill <- plotData$.lineplot_group
  }

  mymapping <- aes(x = .lineplot_x, y = .lineplot_mid)
  if (!is.null(this.group)) {
    mymapping$group <- quote(.lineplot_group)
  }

  myplot <- ggplot(plotData, mapping = mymapping) +
    guides(alpha = "none") +
    labs(title = "Interval-valued Line Plot")

  if (isTRUE(add_line)) {
    myplot <- myplot + geom_line()
  }

  myplot <- myplot + geom_point()

  if (".lineplot_fill" %in% names(plotData)) {
    myplot + geom_crossbar(
      aes(
        ymin = .lineplot_ymin,
        ymax = .lineplot_ymax,
        fill = .lineplot_fill
      ),
      width = barWidth,
      linewidth = 0.25
    )
  } else {
    myplot + geom_errorbar(
      aes(
        ymin = .lineplot_ymin,
        ymax = .lineplot_ymax
      ),
      width = barWidth * 0.4,
      linewidth = 0.25
    )
  }
}

#' @rdname ggInterval_lineplot
#' @export
ggInterval_tsplot <- function(data = NULL,
                              mapping = aes(NULL),
                              barWidth = 0.5,
                              add_line = TRUE) {
  ggInterval_lineplot(
    data = data,
    mapping = mapping,
    barWidth = barWidth,
    add_line = add_line
  )
}

.prepare_lineplot_data <- function(ggSymData) {
  plotData <- as.data.frame(ggSymData$intervalData)
  rawData <- ggSymData$rawData

  if (!is.null(rawData) &&
      is.data.frame(rawData) &&
      nrow(rawData) == nrow(plotData)) {
    extra_names <- setdiff(names(rawData), names(plotData))
    if (length(extra_names) > 0) {
      plotData <- dplyr::bind_cols(plotData, rawData[, extra_names, drop = FALSE])
    }
  }

  plotData
}

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    ".lineplot_fill",
    ".lineplot_group",
    ".lineplot_mid",
    ".lineplot_x",
    ".lineplot_ymax",
    ".lineplot_ymin"
  ))
}
