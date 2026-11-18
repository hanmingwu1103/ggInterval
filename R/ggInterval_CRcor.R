#' Pearson correlations of interval centers and full ranges
#'
#' Compare associations among interval centers and full ranges, optionally
#' separating pooled correlations from correlations after removing group means.
#'
#' @param data An interval input accepted by ggInterval: a symbolic
#'   table, an object with an `intervalData` component, or a list containing
#'   numeric `lower` and `upper` matrices.
#' @param variables Optional character vector selecting interval variables.
#' @param group Optional vector of group labels, with one nonmissing value per
#'   observation. Group means are removed separately from every derived column.
#' @param labels Logical scalar. Print correlations to two decimal places on
#'   the tiles. Diagnostic values retain their full numerical precision.
#'
#' @details For variable \eqn{j}, \eqn{C_j=(a_j+b_j)/2} is its center and
#'   \eqn{R_j=b_j-a_j} is its full range. Derived columns are ordered
#'   `C1`, ..., `Cp`, `R1`, ..., `Rp`. Rows run from top to bottom in this same
#'   order, so the lower-left triangle displays pooled Pearson correlations.
#'   When `group` is supplied, the upper-right triangle displays Pearson
#'   correlations of residuals obtained by subtracting each group's column means.
#'   Otherwise both triangles display pooled correlations. Diagonal entries
#'   contain pooled self-correlations: one for a varying column, `NA` otherwise.
#'
#'   Group-demeaned correlations are descriptive associations among the residuals
#'   pooled over observations, not averages of group-specific correlations.
#'   They are not intraclass correlations (ICCs), tests of group effects, or an
#'   implementation of the B, BD, or BG interval-correlation estimators. Constant
#'   columns have undefined correlations, shown as grey `NA` tiles. Singleton
#'   groups contribute zero residuals and trigger a warning. If all derived
#'   columns have zero residual variance, an informative error is raised.
#'
#' @return An ordinary `ggplot` object with an `interval_diagnostics` attribute.
#'   It contains the derived `values`, `pooled` and `group_demeaned` correlation
#'   matrices, `residuals`, the mixed `displayed` matrix, a `cells` table, a
#'   `variables` key, and `settings`. Undefined correlations remain `NA`.
#' @export
#' @examples
#' library(ggInterval)
#' x <- list(lower = cbind(X = c(0, 2, 5)),
#'           upper = cbind(X = c(2, 5, 9)))
#' ggInterval_CRcor(x)
ggInterval_CRcor <- function(data, variables = NULL, group = NULL,
                             labels = TRUE) {
  if (!is.logical(labels) || length(labels) != 1L || is.na(labels)) {
    stop("`labels` must be TRUE or FALSE.", call. = FALSE)
  }
  input <- .ggInterval_endpoint_inputs(data, variables)
  if (!is.null(group)) {
    if (!is.atomic(group) || !is.null(dim(group)) ||
        length(group) != input$n || anyNA(group)) {
      stop("`group` must be a vector of length n with no missing values.",
           call. = FALSE)
    }
  }
  values <- cbind(input$lower / 2 + input$upper / 2,
                  input$upper - input$lower)
  if (any(!is.finite(values))) {
    stop("Computed centers and ranges must be finite; rescale the input.",
         call. = FALSE)
  }
  keys <- c(paste0("C", seq_len(input$p)), paste0("R", seq_len(input$p)))
  colnames(values) <- keys
  rownames(values) <- input$ids
  variable_key <- data.frame(
    key = keys, variable = rep(input$variables, 2L),
    component = rep(c("center", "full range"), each = input$p)
  )
  pearson <- function(x) {
    answer <- matrix(NA_real_, ncol(x), ncol(x),
                     dimnames = list(colnames(x), colnames(x)))
    varying <- vapply(seq_len(ncol(x)), function(j) {
      nrow(x) > 1L && any(x[, j] != x[1L, j])
    }, logical(1))
    if (any(varying)) {
      answer[varying, varying] <- stats::cor(x[, varying, drop = FALSE])
    }
    answer
  }
  pooled <- pearson(values)
  adjusted <- residuals <- NULL
  group_sizes <- NULL
  if (!is.null(group)) {
    groups <- factor(group, levels = unique(group))
    rows <- split(seq_len(input$n), groups, drop = TRUE)
    group_sizes <- lengths(rows)
    residuals <- values
    for (indices in rows) {
      residuals[indices, ] <- sweep(values[indices, , drop = FALSE], 2L,
                                    colMeans(values[indices, , drop = FALSE]))
    }
    adjusted <- pearson(residuals)
    if (all(is.na(adjusted))) {
      stop(paste("No within-group variation remains after subtracting group",
                 "means; group-demeaned correlations are undefined."),
           call. = FALSE)
    }
    if (any(group_sizes == 1L)) {
      warning(paste("Singleton groups contribute zero group-demeaned residuals;",
                    "the upper triangle pools residual variation from the",
                    "remaining groups."), call. = FALSE)
    }
  }
  displayed <- pooled
  if (!is.null(adjusted)) displayed[upper.tri(displayed)] <- adjusted[upper.tri(adjusted)]
  cells <- expand.grid(row_index = seq_along(keys),
                       column_index = seq_along(keys))
  cells$row <- factor(keys[cells$row_index], levels = keys)
  cells$column <- factor(keys[cells$column_index], levels = keys)
  cells$correlation <- displayed[cbind(cells$row_index, cells$column_index)]
  cells$triangle <- ifelse(cells$row_index == cells$column_index, "diagonal",
                           ifelse(cells$row_index > cells$column_index,
                                  "lower: pooled",
                                  if (is.null(group)) "upper: pooled" else
                                    "upper: group-demeaned"))
  cells$label <- ifelse(is.na(cells$correlation), "NA",
                        sprintf("%.2f", cells$correlation))
  cells$label_colour <- ifelse(!is.na(cells$correlation) &
                                abs(cells$correlation) > 0.6,
                               "white", "#222222")
  axis_labels <- stats::setNames(
    paste0(keys, " (", rep(input$variables, 2L), ")"), keys
  )
  caption <- if (is.null(group)) {
    "Both triangles: pooled Pearson correlations."
  } else {
    "Lower-left: pooled Pearson; upper-right: group-demeaned Pearson."
  }
  caption <- paste(caption,
                    "C = center; R = full range. Diagonal: pooled; grey: zero variance.",
                    "Correlations are descriptive; this is not an ICC or BG matrix.",
                    sep = "\n")
  p <- ggplot2::ggplot(
    cells, ggplot2::aes(x = .data$column, y = .data$row,
                        fill = .data$correlation)
  ) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.3) +
    ggplot2::scale_fill_gradient2(
      low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0,
      limits = c(-1, 1), na.value = "#D9D9D9", name = "Pearson r"
    ) +
    ggplot2::scale_x_discrete(limits = keys, labels = axis_labels) +
    ggplot2::scale_y_discrete(limits = rev(keys), labels = axis_labels) +
    ggplot2::coord_equal() +
    ggplot2::labs(x = "Derived interval components", y = NULL,
                  caption = caption) + .ggInterval_diagnostic_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    
  if (labels) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = .data$label, colour = .data$label_colour),
      size = 3, show.legend = FALSE
    ) + ggplot2::scale_colour_identity()
  }
  attr(p, "interval_diagnostics") <- list(
    values = values, pooled = pooled, group_demeaned = adjusted,
    residuals = residuals, displayed = displayed, cells = cells,
    variables = variable_key,
    settings = list(variables = input$variables, n = input$n, group = group,
                    group_sizes = group_sizes, labels = labels,
                    lower_triangle = "pooled Pearson",
                    upper_triangle = if (is.null(group)) "pooled Pearson" else
                      "Pearson after removing group means",
                    diagonal = "pooled Pearson", range = "full width b-a")
  )
  p
}
