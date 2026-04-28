match_hist2d_method <- function(method) {
  match.arg(method, c("equal-bin", "unequal-bin"))
}

match_hist2d_display <- function(display) {
  match.arg(display, c("p", "f", "h"))
}

hist2d_legend_name <- function(display) {
  switch(
    display,
    f = "weighted freq",
    p = "relative freq",
    h = "density"
  )
}

hist2d_padding <- function(value) {
  if (!is.finite(value) || value == 0) {
    return(0.5)
  }
  max(abs(value) * 0.05, 0.5)
}

merge_hist2d_breaks <- function(breaks, tau) {
  tau <- as.numeric(tau)
  if (length(tau) != 1 || !is.finite(tau) || tau < 0) {
    stop("ERROR : tau must be a single non-negative number.")
  }
  if (tau == 0 || length(breaks) <= 2) {
    return(breaks)
  }

  groups <- list()
  current_group <- breaks[1]
  for (idx in 2:length(breaks)) {
    if ((breaks[idx] - breaks[idx - 1]) < tau) {
      current_group <- c(current_group, breaks[idx])
    } else {
      groups[[length(groups) + 1]] <- current_group
      current_group <- breaks[idx]
    }
  }
  groups[[length(groups) + 1]] <- current_group

  merged <- vapply(
    seq_along(groups),
    FUN.VALUE = numeric(1),
    FUN = function(i) {
      group <- groups[[i]]
      if (i == 1) {
        min(group)
      } else if (i == length(groups)) {
        max(group)
      } else {
        mean(group)
      }
    }
  )
  merged <- sort(unique(merged))

  if (length(merged) < 2) {
    return(c(min(breaks), max(breaks)))
  }
  merged
}

build_hist2d_breaks <- function(interval_column, bins, method, tau = 0) {
  mins <- interval_column$min
  maxs <- interval_column$max
  lower <- min(mins)
  upper <- max(maxs)

  if (method == "equal-bin") {
    if (bins < 1) {
      stop("ERROR : The number of bins must be at least 1.")
    }
    if (lower == upper) {
      pad <- hist2d_padding(lower)
      return(seq(lower - pad, upper + pad, length.out = bins + 1))
    }
    return(seq(lower, upper, length.out = bins + 1))
  }

  breaks <- sort(unique(c(mins, maxs)))
  breaks <- merge_hist2d_breaks(breaks, tau = tau)
  if (length(breaks) < 2) {
    pad <- hist2d_padding(lower)
    breaks <- c(lower - pad, upper + pad)
  }
  breaks
}

interval_overlap_ratio <- function(lower, upper, breaks) {
  left <- breaks[-length(breaks)]
  right <- breaks[-1]

  if (upper - lower == 0) {
    return(as.numeric(lower >= left & lower <= right))
  }

  overlap <- pmin(right, upper) - pmax(left, lower)
  overlap[overlap < 0] <- 0
  overlap / (upper - lower)
}

build_hist2d_frequency_matrix <- function(x_interval, y_interval, x_breaks, y_breaks) {
  x_cells <- length(x_breaks) - 1
  y_cells <- length(y_breaks) - 1
  n <- length(x_interval$min)
  freq_rectangle <- matrix(0, nrow = x_cells, ncol = y_cells)

  for (obs in seq_len(n)) {
    fx <- interval_overlap_ratio(
      lower = x_interval$min[obs],
      upper = x_interval$max[obs],
      breaks = x_breaks
    )
    fy <- interval_overlap_ratio(
      lower = y_interval$min[obs],
      upper = y_interval$max[obs],
      breaks = y_breaks
    )

    if (any(fx > 0) && any(fy > 0)) {
      freq_rectangle <- freq_rectangle + tcrossprod(fx, fy)
    }
  }

  freq_rectangle
}

build_hist2d_frame <- function(freq_rectangle, x_breaks, y_breaks, n) {
  grid <- expand.grid(
    x_index = seq_len(length(x_breaks) - 1),
    y_index = seq_len(length(y_breaks) - 1),
    KEEP.OUT.ATTRS = FALSE
  )

  grid$f <- c(freq_rectangle)
  grid$x1 <- x_breaks[grid$x_index]
  grid$x2 <- x_breaks[grid$x_index + 1]
  grid$y1 <- y_breaks[grid$y_index]
  grid$y2 <- y_breaks[grid$y_index + 1]
  grid$area <- (grid$x2 - grid$x1) * (grid$y2 - grid$y1)
  grid$p <- grid$f / n
  grid$h <- ifelse(grid$area > 0, grid$p / grid$area, 0)
  grid$xmid <- (grid$x1 + grid$x2) / 2
  grid$ymid <- (grid$y1 + grid$y2) / 2

  grid[, c("f", "p", "h", "x1", "x2", "y1", "y2", "area", "xmid", "ymid")]
}

build_hist2d_data <- function(iData, attr1, attr2, xBins, yBins, method, tau = 0) {
  x_breaks <- build_hist2d_breaks(iData[[attr1]], xBins, method, tau = tau)
  y_breaks <- build_hist2d_breaks(iData[[attr2]], yBins, method, tau = tau)
  freq_rectangle <- build_hist2d_frequency_matrix(
    x_interval = iData[[attr1]],
    y_interval = iData[[attr2]],
    x_breaks = x_breaks,
    y_breaks = y_breaks
  )

  list(
    xBreaks = x_breaks,
    yBreaks = y_breaks,
    xBins = length(x_breaks) - 1,
    yBins = length(y_breaks) - 1,
    freq = freq_rectangle,
    data = build_hist2d_frame(freq_rectangle, x_breaks, y_breaks, n = nrow(iData))
  )
}

hist2d_assign_display <- function(hist2d_frame, display) {
  display <- match_hist2d_display(display)
  hist2d_frame$display_value <- hist2d_frame[[display]]
  hist2d_frame$display_metric <- display
  hist2d_frame
}

format_hist2d_labels <- function(values) {
  gsub(
    pattern = "^0",
    replacement = "",
    x = format(round(values, 2), trim = TRUE, nsmall = 2)
  )
}

build_hist2d_table <- function(hist2d_data, attr1, attr2, display = "f") {
  display <- match_hist2d_display(display)
  metric_matrix <- matrix(
    round(hist2d_data$data[[display]], 3),
    nrow = hist2d_data$xBins,
    ncol = hist2d_data$yBins
  )

  c_names <- paste0(
    "[",
    paste(
      round(hist2d_data$yBreaks[-length(hist2d_data$yBreaks)], 2),
      round(hist2d_data$yBreaks[-1], 2),
      sep = ":"
    ),
    "]"
  )
  r_names <- paste0(
    "[",
    paste(
      round(hist2d_data$xBreaks[-length(hist2d_data$xBreaks)], 2),
      round(hist2d_data$xBreaks[-1], 2),
      sep = ":"
    ),
    "]"
  )

  if (display == "h") {
    rownames(metric_matrix) <- r_names
    colnames(metric_matrix) <- c_names
    return(as.data.frame(metric_matrix))
  }

  s1 <- apply(metric_matrix, 1, sum)
  s2 <- apply(metric_matrix, 2, sum)
  total_value <- sum(metric_matrix)
  metric_matrix <- cbind(metric_matrix, s1)
  metric_matrix <- rbind(metric_matrix, c(s2, total_value))

  metric_label <- hist2d_legend_name(display)
  rownames(metric_matrix) <- c(r_names, paste0(metric_label, " of ", attr1))
  colnames(metric_matrix) <- c(c_names, paste0(metric_label, " of ", attr2))

  as.data.frame(metric_matrix)
}
