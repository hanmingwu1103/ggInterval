#' @name ggInterval_boxplot
#' @title A interval Box plot
#' @description  Visualize the one continuous variable distribution using one of
#' three interval-aware boxplot styles.
#' @import rlang ggplot2
#' @importFrom RSDA is.sym.interval
#' @param data A ggInterval object. It can also be either RSDA object or
#' classical data frame, which will be automatically convert to ggInterval
#' data.
#' @param mapping Set of aesthetic mappings created by aes() or aes_().
#' If specified and inherit. aes = TRUE (the default),
#' it is combined with the default mapping at the top level of
#' the plot. You must supply mapping if there is no plot mapping.
#' It is the same as the mapping of ggplot2.
#' @param plotAll booleans, if TRUE, plot all variable together
#' @param width_type Box-width style. Use \code{"violin-like"} (default) to let
#' the rectangle widths reflect local interval concentration, 
#' \code{"quantile-depth"} for progressively narrower quantile boxes, or
#' \code{"side-by-side"} for conventional boxplots of the lower and upper
#' bounds shown next to each other.
#' @return Return a ggplot2 object.
#' @usage ggInterval_boxplot(data = NULL,mapping = aes(NULL),plotAll=FALSE,
#' width_type = "violin-like")
#' @examples
#' mydata <- ggInterval::facedata
#' ggInterval_boxplot(mydata, aes(x = AD))
#' ggInterval_boxplot(mydata, aes(x = AD), width_type = "quantile-depth")
#' ggInterval_boxplot(mydata, plotAll = TRUE, width_type = "side-by-side")
#'
#' @export
ggInterval_boxplot <- function(data = NULL,
                               mapping = aes(NULL),
                               plotAll = FALSE,
                               width_type = "violin-like") {
  width_type <- match.arg(width_type, c("quantile-depth", "violin-like", "side-by-side"))
  argsNum <- length(mapping)
  args <- lapply(mapping[1:argsNum], FUN = rlang::get_expr)
  this.x <- args$x
  this.y <- args$y
  
  ggSymData <- testData(data)
  iData <- ggSymData$intervalData
  if (!plotAll) {
    testXY(iData, this.x, this.y)
  }
  p <- dim(iData)[2]
  
  with(data, {
    # build box
    if (plotAll) {
      #get numerical data
      numericData <- unlist(lapply(data.frame(iData[1:dim(iData)[2]]) , FUN = is.sym.interval))
      iData <- iData[, which(numericData)]
      attr <- colnames(iData)
    } else{
      #get attr
      if (any(unlist(lapply(
        as.data.frame(data[, 1:p]),
        FUN = identical,
        x = eval(this.x)
      )))) {
        attr <- which(unlist(lapply(
          as.data.frame(data[, 1:p]),
          FUN = identical,
          x = eval(this.x)
        )))
        attr <- names(attr)
      } else if (any(unlist(lapply(
        as.data.frame(data[, 1:p]),
        FUN = identical,
        x = eval(this.y)
      )))) {
        attr <- which(unlist(lapply(
          as.data.frame(data[, 1:p]),
          FUN = identical,
          x = eval(this.y)
        )))
        attr <- names(attr)
      } else{
        stop("ERROR : Cannot find variables in aes(...)")
      }
      if (p == 1) {
        attr = colnames(data)
      }
      #test attribute illegal
      if (all(!is.numeric(data[[attr]]) , !RSDA::is.sym.interval(data[[attr]]))) {
        stop("ERROR : Variables in Box Plot can only be numeric.")
      }
    }

    #plot
    if (is.null(this.x) || is.null(this.y)) {
      mymapping <- mapping[-1]
    } else{
      mymapping <- mapping[-c(1, 2)]
    }

    if (width_type == "side-by-side") {
      d <- build_side_by_side_box_data(iData, attr)
      arg1 <- list(
        mapping = aes(
          x = endpoint,
          y = value,
          fill = endpoint
        ),
        width = 0.6
      )
      arg2 <- mymapping
      ggArgs <- as.list(structure(as.expression(c(arg2, arg1)), class = "uneval"))

      base <- ggplot(data = d) + do.call(geom_boxplot, ggArgs) +
        scale_fill_manual(
          name = "bound",
          values = c("min" = "gray35", "max" = "gray80"),
          labels = c("min" = "Minimum", "max" = "Maximum")
        ) +
        guides(colour = "none", alpha = "none") +
        labs(y = "Values")

      if (plotAll) {
        base <- base + facet_grid(. ~ var)
      } else{
        base <- base + labs(x = attr)
      }
      return(base)
    }

    d <- build_quantile_box_data(iData, attr, width_type)
    d$group <- as.factor(d$group)

    arg1 <- list(
      mapping = aes(
        xmin = x1,
        xmax = x2,
        ymin = y1,
        ymax = y2,
        fill = group
      ),
      col = "black"
    )
    arg2 <- mymapping
    ggArgs <- as.list(structure(as.expression(c(arg2, arg1)), class = "uneval"))

    base  <- ggplot(data = d) + do.call(geom_rect, ggArgs) +
      scale_fill_manual(
        name = "quantile level",
        values = gray.colors(5, start = 0.25, end = 0.85),
        labels = c("0%", "25%", "50%", "75%", "100%")
      ) +
      guides(colour = "none", alpha = "none") +
      scale_x_continuous(limits = c(-0.5, 2.5)) +
      labs(y = "Values")

    if (plotAll) {
      base <- base + facet_grid(. ~ var)
    } else{
      base <- base + labs(x = attr)
    }
    return(base)
  })
}

build_quantile_box_data <- function(iData, attr, width_type = c("quantile-depth", "violin-like")) {
  width_type <- match.arg(width_type)
  quantileN <- 5
  xmid <- rep(1, quantileN)
  d <- NULL

  for (i in seq_along(attr)) {
    mins <- iData[[attr[i]]]$min
    maxs <- iData[[attr[i]]]$max
    y1 <- as.numeric(quantile(mins))
    y2 <- as.numeric(quantile(maxs))
    widths <- switch(
      width_type,
      "quantile-depth" = seq(0.5, 0.3, length.out = quantileN),
      "violin-like" = compute_violin_like_widths(mins, maxs, y1, y2)
    )
    temp <- data.frame(
      x1 = xmid - widths,
      x2 = xmid + widths,
      y1 = y1,
      y2 = y2,
      var = attr[i],
      group = seq_len(quantileN)
    )
    d <- rbind(d, temp)
  }

  d
}

build_side_by_side_box_data <- function(iData, attr) {
  d <- NULL
  for (i in seq_along(attr)) {
    temp <- data.frame(
      value = c(iData[[attr[i]]]$min, iData[[attr[i]]]$max),
      endpoint = factor(
        c(rep("min", length(iData[[attr[i]]]$min)), rep("max", length(iData[[attr[i]]]$max))),
        levels = c("min", "max")
      ),
      var = attr[i]
    )
    d <- rbind(d, temp)
  }
  d
}

compute_violin_like_widths <- function(mins, maxs, y1, y2,
                                       display_min_width = 0.3,
                                       display_max_width = 0.5) {
  scores <- mapply(
    FUN = function(lower, upper) average_interval_density(mins, maxs, lower, upper),
    lower = y1,
    upper = y2
  )
  rescale_box_widths(
    scores,
    display_min_width = display_min_width,
    display_max_width = display_max_width
  )
}

average_interval_density <- function(mins, maxs, lower, upper, grid_n = 25) {
  if (!is.finite(lower) || !is.finite(upper)) {
    return(NA_real_)
  }
  if (lower == upper) {
    return(interval_density(lower, mins, maxs))
  }
  xs <- seq(lower, upper, length.out = grid_n)
  mean(vapply(xs, interval_density, numeric(1), mins = mins, maxs = maxs))
}

interval_density <- function(value, mins, maxs) {
  lens <- pmax(maxs - mins, sqrt(.Machine$double.eps))
  sum((value >= mins & value <= maxs) / lens)
}

rescale_box_widths <- function(scores,
                               display_min_width = 0.3,
                               display_max_width = 0.5) {
  finite_scores <- scores[is.finite(scores)]
  if (length(finite_scores) == 0 || diff(range(finite_scores)) == 0) {
    return(rep((display_min_width + display_max_width) / 2, length(scores)))
  }
  scaled <- (scores - min(finite_scores)) / diff(range(finite_scores))
  scaled[!is.finite(scaled)] <- 0.5
  display_min_width + scaled * (display_max_width - display_min_width)
}
