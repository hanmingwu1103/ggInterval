# Internal endpoint extraction and plotting theme for diagnostic graphics.
# Inputs are validated without aggregation or implicit missing-value deletion.

.ggInterval_endpoint_inputs <- function(data, variables = NULL) {
  wrapped <- is.list(data) || (is.environment(data) && inherits(data, "ggInterval"))
  if (wrapped && !is.null(data[["intervalData"]])) {
    data <- data[["intervalData"]]
  }
  if (is.list(data) && !is.null(data[["lower"]]) &&
      !is.null(data[["upper"]])) {
    lower <- as.matrix(data[["lower"]])
    upper <- as.matrix(data[["upper"]])
    if (!identical(dim(lower), dim(upper))) {
      stop("lower and upper must have identical dimensions.", call. = FALSE)
    }
    if (!is.numeric(lower) || !is.numeric(upper)) {
      stop("Endpoints must be numeric.", call. = FALSE)
    }
    nm <- colnames(lower)
    if (is.null(nm)) nm <- colnames(upper)
    if (is.null(nm)) nm <- paste0("X", seq_len(ncol(lower)))
    if (!is.null(colnames(lower)) && !is.null(colnames(upper)) &&
        !identical(colnames(lower), colnames(upper))) {
      stop("lower and upper column names must agree in order.", call. = FALSE)
    }
    if (!is.null(rownames(lower)) && !is.null(rownames(upper)) &&
        !identical(rownames(lower), rownames(upper))) {
      stop("lower and upper row names must agree in order.", call. = FALSE)
    }
    ids <- rownames(lower)
    if (is.null(ids)) ids <- rownames(upper)
  } else if (is.data.frame(data) || is.list(data)) {
    all_names <- names(data)
    if (is.null(all_names)) stop("Interval columns must be named.", call. = FALSE)
    interval <- vapply(data, function(x) {
      tryCatch(is.numeric(x$min) && is.numeric(x$max),
               error = function(e) FALSE)
    }, logical(1))
    nm <- all_names[interval]
    if (!length(nm)) {
      stop("Supply interval columns or list(lower = ..., upper = ...).", call. = FALSE)
    }
    lengths <- vapply(data[nm], function(x) length(x$min), integer(1))
    upper_lengths <- vapply(data[nm], function(x) length(x$max), integer(1))
    if (length(unique(c(lengths, upper_lengths))) != 1L) {
      stop("All endpoint columns must have the same length.", call. = FALSE)
    }
    lower <- matrix(unlist(lapply(data[nm], function(x) x$min), use.names = FALSE),
                    ncol = length(nm))
    upper <- matrix(unlist(lapply(data[nm], function(x) x$max), use.names = FALSE),
                    ncol = length(nm))
    ids <- rownames(data)
  } else {
    stop("Unsupported data; use an interval table or lower/upper matrices.", call. = FALSE)
  }
  if (!nrow(lower) || !ncol(lower)) stop("At least one observation and variable are required.", call. = FALSE)
  if (anyNA(nm) || any(!nzchar(nm)) || anyDuplicated(nm)) {
    stop("Variable names must be nonmissing, nonempty and unique.", call. = FALSE)
  }
  colnames(lower) <- colnames(upper) <- nm
  if (!is.null(variables)) {
    if (!is.character(variables) || !length(variables) || anyNA(variables) ||
        anyDuplicated(variables) || !all(variables %in% nm)) {
      stop("variables must select unique existing interval column names.", call. = FALSE)
    }
    lower <- lower[, variables, drop = FALSE]
    upper <- upper[, variables, drop = FALSE]
    nm <- variables
  }
  if (any(!is.finite(lower)) || any(!is.finite(upper))) {
    stop("All selected endpoints must be finite; missing values are not dropped.", call. = FALSE)
  }
  if (any(lower > upper)) stop("Each lower endpoint must be <= its upper endpoint.", call. = FALSE)
  if (any(!is.finite(upper - lower))) {
    stop("Endpoint spans exceed numerical range; rescale the input.", call. = FALSE)
  }
  if (is.null(ids)) ids <- as.character(seq_len(nrow(lower)))
  if (length(ids) != nrow(lower) || anyNA(ids) || anyDuplicated(ids)) {
    stop("Observation identifiers must be unique and nonmissing.", call. = FALSE)
  }
  rownames(lower) <- rownames(upper) <- as.character(ids)
  list(lower = lower, upper = upper, variables = nm, ids = as.character(ids),
       n = nrow(lower), p = ncol(lower))
}

.ggInterval_diagnostic_theme <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "sans") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", colour = "#172B3A"),
      plot.subtitle = ggplot2::element_text(colour = "#4C5E6B", margin = ggplot2::margin(b = 8)),
      plot.caption = ggplot2::element_text(hjust = 0, colour = "#52616B", size = ggplot2::rel(0.8)),
      plot.margin = ggplot2::margin(10, 12, 10, 10),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = ggplot2::rel(0.9))
    )
}
