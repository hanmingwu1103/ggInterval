#' @name ggInterval_CRplot
#' @title Center-range plot for interval data
#' @description  Visualize the relation between interval centers and ranges,
#' with mean reference lines and an optional ellipse overlay.
#' @import ggplot2
#' @importFrom RSDA is.sym.interval
#' @param data A ggInterval object. It can also be either RSDA object or
#' classical data frame,which will be automatically convert to ggInterval
#' data.
#' @param mapping Set of aesthetic mappings created by aes() or aes_().
#' If specified and inherit. aes = TRUE (the default),
#' it is combined with the default mapping at the top level of
#' the plot. You must supply mapping if there is no plot mapping.
#' It is the same as the mapping of ggplot2.
#' @param plotAll booleans, if TRUE, plot all variable together
#' @param addEllipse logical, if TRUE (default), add a shaded ellipse layer.
#' @param ellipseFill fill color of the ellipse layer. Default is "blue".
#' @param ellipseAlpha alpha level of the ellipse layer. Default is 0.3.
#' @return Return a ggplot2 object.
#' @usage ggInterval_CRplot(data = NULL,mapping = aes(NULL),plotAll=FALSE,
#'                          addEllipse = TRUE,
#'                          ellipseFill = "blue",
#'                          ellipseAlpha = 0.3)
#' @examples
#' mydata <- ggInterval::facedata
#' ggInterval_CRplot(mydata, aes(x = AD, col = "blue", shape = 2))
#' ggInterval_CRplot(mydata, plotAll = TRUE)
#' ggInterval_CRplot(mydata, plotAll = TRUE, addEllipse = FALSE)
#' @export
ggInterval_CRplot <- function(data = NULL,
                              mapping = aes(NULL),
                              plotAll = FALSE,
                              addEllipse = TRUE,
                              ellipseFill = "blue",
                              ellipseAlpha = 0.3) {
  #data preparing
  argsNum <- length(mapping)
  args <- lapply(mapping[1:argsNum], FUN = rlang::get_expr)
  this.x <- args$x
  this.y <- args$y
  
  #test data illegal
  ggSymData <- testData(data)
  iData <- ggSymData$intervalData
  testXY(iData, this.x, this.y)
  p <- dim(iData)[2]
  
  #start process
  with(data, {
    if (plotAll) {
      #get numerical data
      numericData <- unlist(lapply(data.frame(iData[1:dim(iData)[2]]) , FUN = is.sym.interval))
      iData <- iData[, which(numericData)]
      attr <- colnames(iData)
      d <- NULL
      #build data frame
      for (i in 1:length(attr)) {
        #build data frame for ggplot
        center <- (iData[[attr[i]]]$min + iData[[attr[i]]]$max) / 2
        r <- (iData[[attr[i]]]$max - iData[[attr[i]]]$min)
        temp <- data.frame(
          cbind(center, r),
          minCenter = min(center),
          maxCenter = max(center),
          minRange = min(r),
          maxRange = max(r),
          meanC = mean(center),
          meanR = mean(r),
          var = attr[i]
        )
        d <- rbind(d, temp)
        #xmid <- xmid + 1
      }
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
        stop("ERROR : Variables in Min-Max Plot can only be numeric.")
      }
      
      #build data frame for ggplot
      center <- (iData[[attr]]$min + iData[[attr]]$max) / 2
      r <- (iData[[attr]]$max - iData[[attr]]$min)
      d <- data.frame(cbind(center, r), var = attr)
      #figure limits
      minCenter <- min(d$center)
      maxCenter <- max(d$center)
      minRange <- min(d$r)
      maxRange <- max(d$r)
      meanC <- mean(d$center)
      meanR <- mean(d$r)
    }
    
    #build Aesthetic (mapping)
    xyLocation <- c(which(names(mapping) == "x"), which(names(mapping) == "y"))
    if (length(xyLocation) != 0) {
      usermapping <- mapping[-xyLocation] #Aesthetic without x,y
    } else{
      usermapping <- mapping
    }
    #usermapping <- mapping[-1] #Aesthetic without x,y
    mymapping <- list(mapping = aes(size = 4))
    allmapping <- as.list(structure(as.expression(c(
      usermapping, mymapping
    )), class = "uneval"))
    
    #add facets
    if (!plotAll) {
      d <- addFactor(rawData = data, iData = d)
    }
    
    #plot
    base <- ggplot(d, aes(center, r, group = var)) +
      do.call(geom_point, allmapping) +
      geom_vline(aes(xintercept = meanC), lty = 2) +
      geom_hline(aes(yintercept = meanR), lty = 2) +
      labs(x = "center", y = "range") +
      guides(size = "none")
    if (isTRUE(addEllipse)) {
      base <- base + stat_ellipse(
        geom = "polygon",
        fill = ellipseFill,
        colour = ellipseFill,
        alpha = ellipseAlpha,
        linewidth = 0.35
      )
    }
    if (plotAll) {
      base <- base + facet_grid(. ~ var)
    }
    return(base)
  })
}
