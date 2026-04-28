#' @name ggInterval_corrplot
#' @title Correlation heatmap for interval-valued data
#' @description Visualize pairwise symbolic correlations between
#' interval-valued variables with a heatmap. This plot is especially useful
#' for summarizing multivariate dependence structures before more formal
#' modeling or dimension reduction.
#' @import ggplot2
#' @importFrom RSDA is.sym.interval
#' @param data A ggInterval object. It can also be either an RSDA object or
#' a classical data frame, which will be automatically converted to ggInterval
#' data.
#' @param method Correlation method for interval-valued variables. It must be
#' one of \code{"centers"} (interval centers), \code{"B"} (Billard),
#' \code{"BD"} (Billard-Diday), or \code{"BG"} (Bertrand-Goupil).
#' @param triangle Which part of the symmetric correlation matrix to display.
#' It can be \code{"lower"}, \code{"upper"}, or \code{"full"}.
#' @param showValues Logical. If \code{TRUE}, print the correlation values in
#' each cell.
#' @param digits Number of digits used when \code{showValues = TRUE}.
#' @param showLegend Logical. If \code{TRUE}, display the fill legend.
#' @references
#' Bertrand, Patrice and Goupil, Francoise (2000). Descriptive Statistics for
#' Symbolic Data. In Hans-Hermann Bock and Edwin Diday (eds.),
#' \emph{Analysis of Symbolic Data}, pp. 106--124. Berlin and Heidelberg:
#' Springer.
#'
#' Billard, Lynne and Diday, Edwin (2006). \emph{Symbolic Data Analysis:
#' Conceptual Statistics and Data Mining}. Chichester, UK: John Wiley and Sons.
#'
#' Billard, Lynne (2008). Sample covariance functions for complex quantitative
#' data. In \emph{Proceedings of the World IASC Conference}, pp. 157--163,
#' Yokohama, Japan.
#' @return Return a ggplot2 object.
#' @examples
#' ggInterval_corrplot(facedata)
#'
#' ggInterval_corrplot(facedata, method = "BG", triangle = "full")
#' @export
ggInterval_corrplot <- function(data = NULL,
                                method = c("centers", "B", "BD", "BG"),
                                triangle = c("lower", "upper", "full"),
                                showValues = TRUE,
                                digits = 2,
                                showLegend = TRUE) {
  method <- match.arg(method)
  triangle <- match.arg(triangle)
  
  ggSymData <- testData(data)
  iData <- ggSymData$intervalData
  numericData <- unlist(lapply(as.data.frame(iData), FUN = RSDA::is.sym.interval))
  iData <- iData[, numericData, drop = FALSE]
  
  if (ncol(iData) < 2) {
    stop("At least two interval-valued variables are required.")
  }
  
  corrMat <- as.matrix(cor(iData, method = method))
  corrMat[] <- pmax(-1, pmin(1, corrMat))
  diag(corrMat) <- 1
  varNames <- colnames(corrMat)
  
  plotData <- expand.grid(
    Var1 = varNames,
    Var2 = varNames,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  plotData$i <- match(plotData$Var1, varNames)
  plotData$j <- match(plotData$Var2, varNames)
  plotData$Correlation <- corrMat[cbind(plotData$j, plotData$i)]
  
  if (triangle == "lower") {
    plotData <- plotData[plotData$i <= plotData$j, , drop = FALSE]
  } else if (triangle == "upper") {
    plotData <- plotData[plotData$i >= plotData$j, , drop = FALSE]
  }
  
  plotData$Var1 <- factor(plotData$Var1, levels = varNames)
  plotData$Var2 <- factor(plotData$Var2, levels = rev(varNames))
  plotData$Label <- formatC(plotData$Correlation, digits = digits, format = "f")
  
  base <- ggplot(
    plotData,
    aes(x = .data$Var1, y = .data$Var2, fill = .data$Correlation)
  ) +
    geom_tile(color = "white", linewidth = 0.4) +
    scale_fill_gradient2(
      low = "#2166ac",
      mid = "white",
      high = "#b2182b",
      midpoint = 0,
      limits = c(-1, 1),
      name = paste0("Correlation\n(", method, ")")
    ) +
    coord_equal() +
    labs(x = "", y = "") +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title = element_blank()
    )
  
  if (showValues) {
    base <- base + geom_text(aes(label = .data$Label), size = 3)
  }
  
  if (!showLegend) {
    base <- base + guides(fill = "none")
  }
  
  base
}
