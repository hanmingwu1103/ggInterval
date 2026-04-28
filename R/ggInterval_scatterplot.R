#' @name ggInterval_scatterplot
#' @title Scatter plot for two continuous interval variables
#' @description Visualize the distribution of two continuous
#' interval-valued variables with rectangles whose widths and heights
#' represent the corresponding intervals.
#' @import rlang ggplot2
#' @importFrom RSDA is.sym.interval
#' @param data A ggInterval object.It can also be either RSDA object or
#' classical data frame, which will be automatically convert to ggInterval
#' data.
#' @param mapping Set of aesthetic mappings created by aes() or aes_().
#' If specified and inherit. aes = TRUE (the default),
#' it is combined with the default mapping at the top level of
#' the plot. You must supply mapping if there is no plot mapping.
#' @param showLabels Logical. If \code{TRUE}, add row labels to the rectangles.
#' @param labelSize Numeric text size used when \code{showLabels = TRUE}.
#' @param labelPosition Character label position used when
#' \code{showLabels = TRUE}. One of \code{"topright"}, \code{"topleft"},
#' \code{"bottomright"}, or \code{"bottomleft"}.
#' @param labelNudgeX Numeric horizontal adjustment applied to label positions.
#' @param labelNudgeY Numeric vertical adjustment applied to label positions.
#' @param checkOverlap Logical. Passed to \code{geom_text()} to suppress
#' overlapping labels when needed.
#' @param ... Others in ggplot2.
#' @return Return a ggplot2 object.
#' @usage ggInterval_scatterplot(data = NULL,mapping = aes(NULL),
#' showLabels = TRUE, labelSize = 3,
#' labelPosition = "topright", labelNudgeX = 0, labelNudgeY = 0,
#' checkOverlap = FALSE, ...)
#' @examples
#' Subjects <- substr(rownames(facedata), 1, 3)
#' ggInterval_scatterplot(facedata, aes(x = AD, y = BC))
#' ggInterval_scatterplot(
#'   facedata,
#'   aes(x = AD, y = BC, fill = Subjects),
#'   showLabels = TRUE,
#'   labelSize = 2.6,
#'   labelPosition = "topright",
#'   labelNudgeX = 0.8,
#'   labelNudgeY = 0.15,
#'   checkOverlap = TRUE,
#'   col = "black"
#' )
#' p <- ggInterval_scatterplot(facedata[1:10, ], aes(x = AD, y = BC, alpha = 0.2))
#' p + scale_fill_manual(
#'   labels = rownames(facedata)[1:10],
#'   values = rainbow(10),
#'   name = "Group"
#' )
#' @export
ggInterval_scatterplot <- function(data = NULL,
                                   mapping = aes(NULL),
                                   showLabels = TRUE,
                                   labelSize = 3,
                                   labelPosition = "topright",
                                   labelNudgeX = 0,
                                   labelNudgeY = 0,
                                   checkOverlap = FALSE,
                                   ...) {
  #data preparing
  . <- NULL
  caller_env <- parent.frame()
  labelPosition <- match.arg(
    labelPosition,
    choices = c("topright", "topleft", "bottomright", "bottomleft")
  )
  argsNum <- length(mapping)
  args <- lapply(mapping[1:argsNum], FUN = rlang::get_expr)
  this.x <- args$x
  this.y <- args$y
  
  #test data illegal
  ggSymData <- testData(data)
  iData <- ggSymData$intervalData
  p <- dim(data)[2]
  n <- dim(iData)[1]
  myRowNames <- rownames(iData)
  
  #test big o
  if (n > 150 & n <= 2000) {
    warning("It is not recommended for too many observations.")
  } else if (n > 2000) {
    stop("Out of time limits.")
  }
  
  #start process
  with(data, {
    resolve_aes_value <- function(expr, n_expected) {
      if (is.null(expr)) {
        return(NULL)
      }
      value <- tryCatch(
        eval(expr, envir = data, enclos = caller_env),
        error = function(err) NULL
      )
      if (is.null(value)) {
        return(NULL)
      }
      if (!(is.atomic(value) || is.factor(value))) {
        return(NULL)
      }
      if (length(value) == 1) {
        return(rep(value, n_expected))
      }
      if (length(value) != n_expected) {
        stop("ERROR : Aesthetic values must have length 1 or match the number of observations.")
      }
      value
    }
    
    #get attrs
    attr1 <- which(unlist(lapply(
      data[, 1:p], FUN = identical, x = eval(this.x)
    )))
    attr1 <- names(attr1)
    attr2 <- which(unlist(lapply(
      data[, 1:p], FUN = identical, x = eval(this.y)
    )))
    attr2 <- names(attr2)
    #if cannot find attr
    if (length(attr1) == 0 || length(attr2) == 0) {
      stop("ERROR : Missing attributes x or y in data frame.")
    }
    
    #test attribute illegal
    if (all((!is.numeric(data[[attr1]])) , !RSDA::is.sym.interval(data[[attr1]]))
        ||
        all((!is.numeric(data[[attr2]])), !RSDA::is.sym.interval(data[[attr2]]))) {
      stop("ERROR : Variables in Scatter Plot can only be numeric.")
    }
    
    #build data.frame for ggplot
    d <- data.frame(
      x1 = iData[[attr1]]$min,
      x2 = iData[[attr1]]$max,
      y1 = iData[[attr2]]$min,
      y2 = iData[[attr2]]$max
    )
    
    #build Aesthetic (mapping)
    xyLocation <- c(which(names(mapping) == "x"), which(names(mapping) == "y"))
    if (length(xyLocation) != 0) {
      usermapping <- mapping[-xyLocation] #Aesthetic without x,y
    } else{
      usermapping <- mapping
    }
    #add facets
    d <- addFactor(rawData = data, iData = d)
    
    
    fill_expr <- args$fill
    colour_expr <- args$colour
    if (is.null(colour_expr)) {
      colour_expr <- args$color
    }
    if (is.null(colour_expr)) {
      colour_expr <- args$col
    }
    fcLocation <- which(names(usermapping) %in% c("fill", "colour", "color", "col"))
    if (length(fcLocation) != 0) {
      usermapping <- usermapping[-fcLocation] #Aesthetic without fill, col
    }
    fill_value <- resolve_aes_value(fill_expr, n)
    if (!is.null(fill_value)) {
      d$rect_fill <- fill_value
    }
    colour_value <- resolve_aes_value(colour_expr, n)
    if (!is.null(colour_value)) {
      d$rect_colour <- colour_value
    }
    
    mapping_rect <- aes(
      xmin = x1,
      xmax = x2,
      ymin = y1,
      ymax = y2,
      alpha = 0.5
    )
    if (length(usermapping) != 0) {
      mapping_rect <- structure(c(mapping_rect, usermapping), class = "uneval")
    }
    if (!is.null(fill_value)) {
      mapping_rect$fill <- rlang::expr(.data$rect_fill)
    }
    if (!is.null(colour_value)) {
      mapping_rect$colour <- rlang::expr(.data$rect_colour)
    }
    
    #ggplot(data=d,aes(x = x1, y = y1))+
    #  do.call(geom_rect,ss)
    
    #start plot
    base <- ggplot(data = d, aes(x = x1, y = y1)) +
      geom_rect(mapping = mapping_rect, ...) +
      guides(colour = "none", alpha = "none") +
      labs(x = attr1, y = attr2)
    if (!is.null(fill_value)) {
      base <- base + labs(fill = rlang::as_label(fill_expr))
    }
    
    if (showLabels) {
      labelSpec <- switch(
        labelPosition,
        topright = list(x = d$x2, y = d$y2, hjust = 0, vjust = 0),
        topleft = list(x = d$x1, y = d$y2, hjust = 1, vjust = 0),
        bottomright = list(x = d$x2, y = d$y1, hjust = 0, vjust = 1),
        bottomleft = list(x = d$x1, y = d$y1, hjust = 1, vjust = 1)
      )
      d$label_x <- labelSpec$x + labelNudgeX
      d$label_y <- labelSpec$y + labelNudgeY
      d$label_text <- myRowNames
      base <- base +
        geom_text(
          data = d,
          mapping = aes(
            x = .data$label_x,
            y = .data$label_y,
            label = .data$label_text
          ),
          inherit.aes = FALSE,
          size = labelSize,
          check_overlap = checkOverlap,
          hjust = labelSpec$hjust,
          vjust = labelSpec$vjust
        )
    }
    
    base
    
  })
}
