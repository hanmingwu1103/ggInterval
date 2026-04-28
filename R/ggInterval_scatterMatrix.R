#' @name ggInterval_scatterMatrix
#' @title Scatterplot matrix for interval-valued data
#' @description Visualize all continuous interval-valued variables with
#' a matrix of pairwise interval scatterplots. Each off-diagonal panel
#' shows interval rectangles for one variable pair, whereas each diagonal
#' panel displays the variable name. This function automatically filters
#' out non-interval variables and plots all remaining continuous interval
#' variables, so explicit \code{x} and \code{y} mappings are not required.
#' It is not recommended to apply the function to too many variables
#' because the full pairwise matrix becomes computationally expensive and
#' visually crowded.
#' @import rlang ggplot2 tidyverse
#' @importFrom grDevices gray.colors
#' @importFrom grDevices grey.colors
#' @param data A ggInterval object. It can also be either RSDA object or
#' classical data frame,which will be automatically convert to ggInterval
#' data.
#' @param mapping Set of aesthetic mappings created by aes() or aes_().
#' If specified and inherit. aes = TRUE (the default),
#' it is combined with the default mapping at the top level of
#' the plot. You must supply mapping if there is no plot mapping.
#' @param showLegend whether show the legend.
#' @param borderLinewidth Numeric border width used for the interval rectangles.
#' @return Return a ggplot2 object.
#' @usage ggInterval_scatterMatrix(data = NULL, mapping = aes(NULL),
#' showLegend = FALSE, borderLinewidth = 0.08)
#' @examples
#' mydata <- ggInterval::facedata
#' ggInterval_scatterMatrix(mydata[, 1:3])
#' ggInterval_scatterMatrix(
#'   mydata[, 1:3],
#'   aes(fill = "black", alpha = 0.2),
#'   borderLinewidth = 0.15
#' )
#' @export
ggInterval_scatterMatrix <- function(data = NULL,
                                 mapping = aes(NULL),
                                 showLegend = FALSE,
                                 borderLinewidth = 0.08) {
  #data preparing
  . <- NULL
  if (!is.numeric(borderLinewidth) || length(borderLinewidth) != 1 ||
      !is.finite(borderLinewidth) || borderLinewidth < 0) {
    stop("ERROR : borderLinewidth must be a single non-negative number.")
  }
  argsNum <- length(mapping)
  args <- lapply(mapping[1:argsNum], FUN = rlang::get_expr)
  this.x <- args$x
  this.y <- args$y
  #remove user's x,y input,remain Aesthetic
  if ((!is.null(this.x)) && (!is.null(this.y))) {
    #both have value
    usermapping <- mapping[-c(1, 2)]
  } else if ((!is.null(this.x)) || (!is.null(this.y))) {
    #only one value
    usermapping <- mapping[-1]
  }
  
  #test data illegal
  ggSymData <- testData(data)
  iData <- ggSymData$intervalData
  myRowNames <- rownames(iData)
  #preparing data
  p <- dim(iData)[2]
  n <- dim(iData)[1]
  numericData <- unlist(lapply(iData[, 1:p] , FUN = RSDA::is.sym.interval))
  iData <- iData[, numericData, drop = FALSE]
  
  #test big o
  if (p > 8 & p <= 20) {
    warning("It is not recommended number of variables are greater than 8.")
  } else if (p > 20) {
    stop(
      "The number of variables are too large to visualize clearly.
         Suggested input variables less than 4. ex. data[,1:4]"
    )
  }
  if (showLegend == T && n > 50) {
    stop("Suggest set showLegend to FALSE in the situation
         that your observations are large.")
  } else if (showLegend == F && n > 3000) {
    stop("Out of time limits.")
  }
  
  
  #now iData only have numeric data
  if (dim(iData)[2] < dim(data)[2]) {
    p <- dim(iData)[2]
    n <- dim(iData)[1]
    warning("Ignore non-numeric data.")
  }
  
  plotData <- NULL
  for (i in 1:p) {
    for (u in 1:p) {
      if (i != u) {
        plotData <- rbind(
          plotData,
          data.frame(
            x1 = iData[[i]]$min,
            x2 = iData[[i]]$max,
            y1 = iData[[u]]$min,
            y2 = iData[[u]]$max,
            xv = colnames(iData)[i],
            yv = colnames(iData)[u],
            Concepts = myRowNames,
            isPlot = T,
            textXY = 0
          )
        )
      } else{
        plotData <- rbind(
          plotData,
          data.frame(
            x1 = 0,
            x2 = 0,
            y1 = 0,
            y2 = 0,
            xv = colnames(iData)[i],
            yv = colnames(iData)[u],
            Concepts = "no concepts",
            isPlot = F,
            textXY = (max(iData[[i]]$max) + min(iData[[i]]$min)) /
              2
          )
        )
      }
    }
  }
  
  #build Aesthetic (mapping)
  xyLocation <- c(which(names(mapping) == "x"), which(names(mapping) == "y"))
  if (length(xyLocation) != 0) {
    usermapping <- mapping[-xyLocation] #Aesthetic without x,y
  } else{
    usermapping <- mapping
  }
  
  mymapping <- list(
    data = . %>% dplyr::filter(.data$isPlot),
    mapping = aes(
      xmin = .data$x1,
      xmax = .data$x2,
      ymin = .data$y1,
      ymax = .data$y2,
      fill = .data$Concepts,
      alpha = 0.5
    ),
    col = "black",
    linewidth = borderLinewidth
  )
  allmapping <- as.list(structure(as.expression(c(
    usermapping, mymapping
  )), class = "uneval"))
  
  
  
  #start plot
  result <- ggplot(data = plotData, aes(.data$x1, .data$y1)) +
    do.call(geom_rect, allmapping) +
    scale_fill_manual(name = "Concept",
                      values = gray.colors(n),
                      labels = myRowNames) +
    geom_text(
      data = . %>% dplyr::filter(!.data$isPlot),
      aes(
        x = .data$textXY,
        y = .data$textXY,
        label = .data$xv
      ),
      size = 12
    ) +
    guides(colour = "none", alpha = "none") +
    facet_grid(.data$yv ~ .data$xv, scales = "free") +
    labs(x = "", y = "")
  
  if (showLegend) {
    return(result)
  } else{
    return(result + theme(legend.position = "None"))
  }
}
