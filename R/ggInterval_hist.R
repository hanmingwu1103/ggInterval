#' @name ggInterval_hist
#' @title Histogram for symbolic data with equal-bin or unequal-bin.
#' @description  Visualize the continuous variable distribution
#' by dividing the x axis into bins,and calculating the frequency
#' of observation interval in each bin.
#' @import rlang ggplot2 tidyverse
#' @importFrom RSDA is.sym.interval
#' @param data A ggInterval object.It can also be either RSDA object or
#' classical data frame,which will be automatically convert to ggInterval
#' data.
#' @param mapping Set of aesthetic mappings created by aes() or aes_().
#' If specified and inherit. aes = TRUE (the default),
#' it is combined with the default mapping at the top level of
#' the plot. You must supply mapping if there is no plot mapping.
#' It is the same as the mapping of ggplot2.
#' @param method It can be \code{"equal-bin"} (default) or
#' \code{"unequal-bin"}. Equal-bin means the histogram uses equally spaced
#' intervals. Unequal-bin means the bin widths are determined by the data,
#' so the argument \code{bins} is unused in that case.
#' @param bins Finite numeric positive integer scalar for equal-bin mode.
#' Values above 200 warn; values above 1000 error. Ignored in unequal-bin mode.
#' @param plotAll boolean, whether plot all variables, default FALSE.
#' @param position "stack" or "identity"
#' @param alpha fill alpha
#' @return An object of class \code{ggInterval_hist_result}. A direct call
#' displays the histogram, and the returned object stores the ggplot object in
#' \code{$plot} together with the accompanying frequency tables.
#' Tables are named \code{Table <var>} or \code{Table  <var>: <group>}.
#' Equal-bin tables contain \code{Interval}, \code{Observed.Frequency} and
#' \code{Relative.Frequency}; unequal-bin tables contain \code{Interval} and
#' \code{Cumulative.Concepts} (the display name is "Cumulative Concepts").
#' @details For equal-bin mode, let A be the minimum lower endpoint and B the
#' maximum upper endpoint, pooled across all groups. With r bins, breaks are
#' A + g (B - A) / r for g = 0, ..., r, generated with exactly r + 1 values.
#' A positive-width interval contributes its overlap length divided by its
#' width: [1,5] with breaks 0,2,4,6 contributes 1/4, 1/2, 1/4.
#' A point contributes one to its left-closed, right-open bin; the final right
#' endpoint belongs to the final bin. Constant ranges are padded on both sides
#' by max(abs(A) * 0.05, 0.5), including padding 0.5 at zero.
#' Allocation uses unrounded breaks; displayed endpoints are rounded to two
#' decimals and retain bracket labels, which do not indicate endpoint ownership.
#' Endpoints must be finite, non-missing and ordered (lower <= upper).
#' In unequal-bin mode, the unchanged 0.01 grid counts intervals covering each
#' grid location. A constant variable (all intervals equal to one value v) is
#' represented by its coverage count n at the exact value v as a zero-width
#' rectangle (vertical spike), without constructing a grid. The equal-bin
#' padding rule does not apply there. "Cumulative
#' Concepts" are coverage counts, not probabilities, and need not integrate to
#' one. This method ignores bins and does not support grouping.
#' @usage ggInterval_hist(data = NULL,mapping = aes(NULL),method="equal-bin",bins=10,
#'  plotAll = FALSE, position = "identity", alpha = 0.5)
#' @examples
#' mydata <- ggInterval::facedata
#' ggInterval_hist(mydata, aes(x = AD), bins = 10)
#'
#' hist_obj <- ggInterval_hist(mydata, plotAll = TRUE, bins = 10)
#' hist_obj
#' hist_obj$`Table AD`
#'
#' ggInterval_hist(mydata, aes(x = AD), method = "unequal-bin")
#'
#' @export
ggInterval_hist <- function(data = NULL,
                            mapping = aes(NULL),
                            method = "equal-bin",
                            bins = 10,
                            plotAll = FALSE,
                            position = "identity",
                            alpha = 0.5) {
  #data preparing
  argsNum <- length(mapping)
  args <- lapply(mapping[1:argsNum], FUN = rlang::get_expr)
  this.x <- args$x
  this.y <- args$y
  this.fill <- args$fill
  this.group <- args$group
  
  #test data illegal
  ggSymData <- testData(data)
  iData <- ggSymData$intervalData
  if (plotAll) {
    if (!is.null(this.x) | !is.null(this.y)) {
      warning("Using plotAll presentation cannot specify variables.")
    }
  } else{
    testXY(iData, this.x, this.y)
  }
  p <- dim(iData)[2]
  n <- dim(iData)[1]
  resultSet <- NULL
  base <- NULL
  
  #test figure clearly
  if (method == "equal-bin") {
    validate_hist_bins(bins, "bins")
    if (bins > 200 && bins <= 1000) {
      warning("Bins are too large that will lead the figure to be fuzzy.")
    } else if (bins > 1000) {
      stop("Bins are too large that will lead the figure to be fuzzy.")
    }
  }
  
  # group for hist adjust
  this.fill <- eval(this.fill)
  this.group <- eval(this.group)
  isGroup <- FALSE
  myGroup <- 1 # no group (initial)
  fill.group <- rep(1, n) # no group(initial)
  if (length(this.group) == n | length(this.fill) == n) {
    if (method == "equal-bin") {
      isGroup <- TRUE
      if (length(this.fill) == n) {
        myGroup <- unique(this.fill)
        fill.group <- this.fill
      } else{
        myGroup <- unique(this.group)
        fill.group <- this.group
      }
    } else{
      warning("Ignore group of concepts, since the method is not support for group.")
    }
  }
  
  
  if ("ggInterval" %in% class(data)) {
    data <- iData
  }
  #start process
  with(data, {
    plotVarNum <- NULL
    mmd <- data.frame(NULL) #min and max data frame
    if (plotAll) {
      #get numerical data
      numericData <- unlist(lapply(data.frame(iData[1:dim(iData)[2]]) , FUN = is.sym.interval))
      iData <- iData[, which(numericData)]
      attr <- colnames(iData)
      
      #adjust for group
      for (i in 1:length(attr)) {
        for (g in myGroup) {
          select.row <- which(fill.group == g)
          temp <- data.frame(
            minimal = min(iData[select.row, i][[1]]$min),
            maximal = max(iData[select.row, i][[1]]$max),
            fill.group = g,
            this.attr = i
          )
          mmd <- rbind(mmd, temp)
        }
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
        stop("ERROR : Variables in histogram can only be numeric.")
      }
      
      for (g in myGroup) {
        select.row <- which(fill.group == g)
        temp <- data.frame(
          minimal = min(iData[select.row, attr][[1]]$min),
          maximal = max(iData[select.row, attr][[1]]$max),
          fill.group = g,
          this.attr = 1
        )
        mmd <- rbind(mmd, temp)
      }
    }#end plotAll
    
    for (var in attr) {
      validate_hist_endpoints(iData[[var]]$min, iData[[var]]$max)
    }

    #unequal-bin cannot grouping
    if (method == "unequal-bin") {
      plotData <- NULL
      intervalDf = NULL
      for (u in 1:nrow(mmd)) {
        lower <- mmd[u, "minimal"]
        upper <- mmd[u, "maximal"]
        if (lower == upper) {
          v <- lower
          plotData <- rbind(plotData, data.frame(
            x1 = v, x2 = v, y1 = 0, y2 = as.numeric(n), group = factor(attr[u])
          ))
          intervalDf <- rbind(intervalDf, data.frame(
            start = round(v, 2), end = round(v, 2)
          ))
          next
        }
        d <- as.data.frame(iData[[attr[u]]])
        temp <- seq(lower, upper, 0.01)
        f <- matrix(0, nrow = length(temp), ncol = n + 1)
        f[, 1] <- temp
        
        #calculate fraquency
        for (i in 1:n) {
          #start debug version0825-10
          #raw code :
          #start<-which(round(f[,1],2)==round(d[i,1],2))
          #end<-which(round(f[,1],2)==round(d[i,2],2))
          #new code :
          start <- length(which(round(f[, 1], 2) <= round(d[i, 1], 2)))
          end <- length(which(round(f[, 1], 2) <= round(d[i, 2], 2)))
          #end debug
          
          f[start:end, i + 1] <- 1
        }
        
        f <- cbind(temp, apply(f[, -1], 1, FUN = sum))
        
        x <- 1
        y <- f[1, 2]
        for (i in 1:(dim(f)[1] - 1)) {
          if (f[i, 2] != f[i + 1, 2]) {
            y <- append(y, f[i + 1, 2])
            x <- append(x, i)
            x <- append(x, i + 1)
          }
        }
        x <- append(x, length(temp))
        x <- t(matrix(x, nrow = 2))
        x1 <- f[x[, 1], 1]
        x2 <- f[x[, 2], 1]
        plotData <- rbind(plotData, data.frame(
          x1,
          x2,
          y1 = 0,
          y2 = y,
          group = factor(attr[u])
        ))
        
        tempDf <- data.frame(start = rep(NA, length(x1)),
                             end = rep(NA, length(x1)))
        tempDf[1:length(x1), "start"] <- round(x1, 2)
        tempDf[1:length(x1), "end"] <- round(x2, 2)
        intervalDf <- rbind(intervalDf, tempDf)
      }
      
      intervalDf <- intervalDf[order(intervalDf$start), ]
      temp <- paste(intervalDf$start, intervalDf$end, sep = ":")
      nameList <- paste0("[", temp, "]")
      
      midP <- (plotData$x1 + plotData$x2) / 2
      #build Aesthetic (mapping)
      xyLocation <- c(which(names(mapping) == "x"), which(names(mapping) == "y"))
      if (length(xyLocation) != 0) {
        usermapping <- mapping[-xyLocation] #Aesthetic without x,y
      } else{
        usermapping <- mapping
      }
      mymapping <- list(
        mapping = aes(
          xmin = x1,
          xmax = x2,
          ymin = y1,
          ymax = y2
        ),
        fill = "grey",
        col = "black",
        linewidth = 0.2,
        alpha = alpha
      )
      allmapping <- as.list(structure(as.expression(c(
        usermapping, mymapping
      )), class = "uneval"))
      
      #plot
      base <- ggplot(data = plotData, aes(x1, y1)) +
        do.call(geom_rect, allmapping) +
        theme(axis.text.x = element_text(
          angle = 90,
          vjust = 0.5,
          hjust = 1
        ))
      
      if (plotAll) {
        base <- base + facet_grid(group ~ .) +
          labs(x = "", y = "frequency") +
          scale_x_continuous(n.breaks = 8)
      } else{
        base <- base + labs(x = attr, y = "frequency") +
          scale_x_continuous(breaks = midP, labels = nameList)
      }
      
      #make table
      for (var in unique(plotData$group)) {
        this.myhist <- dplyr::filter(plotData, group == var)
        this.nameList <- paste0("[",
                                paste(this.myhist$x1, this.myhist$x2, sep = ":"),
                                "]")
        myName <- paste0("Table ", var)
        resultSet[[myName]] <- data.frame(
          "Interval" = unique(this.nameList),
          "Cumulative Concepts" = this.myhist$y2
        )
        
      }
      
      resultSet[["plot"]] <- base
      return(as_ggInterval_hist_result(resultSet))
      
    } else if (method == "equal-bin") {
      myhist <- NULL
      intervalDf <- NULL
      
      #adjust for groups
      #mmd : ith row : the ith variable's minimal and maximal
      for (i in 1:nrow(mmd)) {
        #adjust for group
        select.row <- which(mmd[i, "fill.group"] == fill.group)
        work.attr <- mmd[i, "this.attr"]
        mmd.temp <- dplyr::filter(mmd, this.attr == work.attr)
        #seperate the attribute into bins
        interval <- build_hist_equal_breaks(
          mmd.temp[, "minimal"], mmd.temp[, "maximal"], bins
        )

        #calculate frequency
        f <- NULL
        
        f <- matrix(nrow = nrow(iData[select.row, attr[work.attr]]), ncol =
                      bins)
        
        for (obs in 1:nrow(iData[select.row, attr[work.attr]])) {
          a <- iData[select.row, attr[work.attr]][[1]][obs]$min
          b <- iData[select.row, attr[work.attr]][[1]][obs]$max
          f[obs, ] <- interval_overlap_ratio(a, b, interval)
        }
        
        nInt <- length(interval)
        interval <- round(interval, 2)
        tempDf <- data.frame(start = rep(NA, nInt - 1),
                             end = rep(NA, nInt - 1))
        tempDf[1:(nInt - 1), "start"] <- interval[1:(nInt - 1)]
        tempDf[1:(nInt - 1), "end"] <- interval[2:nInt]
        intervalDf <- rbind(intervalDf, tempDf)
        
        #build data frame and plot
        myhist <- rbind(
          myhist,
          data.frame(
            interval = interval[1:bins],
            frequency = apply(f, 2, FUN = sum),
            group = factor(attr[work.attr]),
            myFill = mmd[i, "fill.group"]
          )
        )
        
      }
      myhist <- cbind(myhist, intervalDf)
      myhist$myFill <- as.factor(myhist$myFill)
      
      intervalDf <- intervalDf[order(intervalDf$start), ]
      temp <- paste(intervalDf$start, intervalDf$end, sep = ":")
      #temp<-mapply(intervalDf$start,intervalDf$end,FUN=function(x,y) paste(interval[x],interval[y],sep = ":"))
      nameList <- paste0("[", temp, "]")
      
      # if(!isGroup){
      #   myhist$frequency <- myhist$frequency/n
      # }
      
      #build Aesthetic (mapping)
      #
      xyLocation <- c(which(names(mapping) == "x"), which(names(mapping) == "y"))
      if (length(xyLocation) != 0) {
        usermapping <- mapping[-xyLocation] #Aesthetic without x,y
      } else{
        usermapping <- mapping
      }
      
      
      mymapping <- list(
        stat = "identity",
        mapping = aes(alpha = alpha, fill = gray.colors(bins *
                                                          length(attr))),
        col = "black"
      )
      
      
      allmapping <- as.list(structure(as.expression(c(
        usermapping, mymapping
      )), class = "uneval"))
      
      
      #plot
      if (plotAll) {
        if (isGroup) {
          if (position == "identity") {
            base <- ggplot(data = myhist, aes(fill = myhist$myFill)) +
              geom_rect(
                mapping = aes(
                  xmin = start,
                  xmax = end,
                  ymin = 0,
                  ymax = frequency
                ),
                col = "black",
                alpha = alpha
              ) +
              theme(axis.text.x = element_text(
                angle = 90,
                vjust = 0.5,
                hjust = 1
              )) +
              labs(x = "",
                   fill = "Group",
                   y = "Frequency") +
              scale_x_continuous(n.breaks = 8) +
              guides(colour = "none", alpha = "none") +
              facet_grid(group ~ .)
          } else if (position == "stack") {
            myhist <- stackFreq(myhist, myGroup)
            base <- ggplot(data = myhist, aes(fill = myhist$myFill)) +
              geom_rect(
                mapping = aes(
                  xmin = start,
                  xmax = end,
                  ymin = ystart,
                  ymax = yend
                ),
                col = "black",
                alpha = alpha
              ) +
              theme(axis.text.x = element_text(
                angle = 90,
                vjust = 0.5,
                hjust = 1
              )) +
              labs(x = "",
                   fill = "Group",
                   y = "Frequency") +
              scale_x_continuous(n.breaks = 8) +
              guides(colour = "none", alpha = "none") +
              facet_grid(group ~ .)
          } else{
            stop("The position is not implemented.")
          }
          
          #make table
          for (var in unique(myhist$group)) {
            this.var.myhist <- dplyr::filter(myhist, group == var)
            this.nameList <- paste0("[",
                                    paste(this.var.myhist$start, this.var.myhist$end, sep = ":"),
                                    "]")
            for (i in unique(myhist$myFill)) {
              this.myhist <- dplyr::filter(this.var.myhist, myFill == i)
              myName <- paste("Table ", paste(unique(this.myhist$group), i, sep = ": "))
              resultSet[[myName]] <- data.frame(
                "Interval" = unique(this.nameList),
                "Observed.Frequency" = this.myhist$frequency,
                "Relative.Frequency" = this.myhist$frequency /
                  sum(this.myhist$frequency)
              )
            }
          }
        } else{
          mymapping <- list(
            mapping = aes(
              xmin = start,
              xmax = end,
              ymin = 0,
              ymax = frequency
            ),
            fill = "grey",
            col = "black"
          )
          allmapping <- as.list(structure(as.expression(c(
            usermapping, mymapping
          )), class = "uneval"))
          base <- ggplot(data = myhist) +
            do.call(geom_rect, allmapping) +
            facet_grid(group ~ .) +
            labs(x = "", y = "frequency") +
            scale_x_continuous(n.breaks = 8)
          
          #make table
          for (var in unique(myhist$group)) {
            this.myhist <- dplyr::filter(myhist, group == var)
            this.nameList <- paste0("[",
                                    paste(this.myhist$start, this.myhist$end, sep = ":"),
                                    "]")
            myName <- paste0("Table ", var)
            resultSet[[myName]] <- data.frame(
              "Interval" = unique(this.nameList),
              "Observed Frequency" = this.myhist$frequency,
              "Relative Frequency" = this.myhist$frequency /
                sum(this.myhist$frequency)
            )
            
          }
        }
      } else{
        #not plot all
        if (isGroup) {
          if (position == "identity") {
            base <- ggplot(data = myhist, aes(fill = myhist$myFill)) +
              geom_rect(
                mapping = aes(
                  xmin = start,
                  xmax = end,
                  ymin = 0,
                  ymax = frequency
                ),
                col = "black",
                alpha = alpha
              ) +
              theme(axis.text.x = element_text(
                angle = 90,
                vjust = 0.5,
                hjust = 1
              )) +
              labs(x = attr,
                   fill = "Group",
                   y = "Frequency") +
              scale_x_continuous(breaks = (myhist[myhist$myFill == myGroup[1], "start"] + myhist[myhist$myFill == myGroup[1], "end"]) /
                                   2,
                                 labels = unique(nameList)) +
              guides(colour = "none", alpha = "none")
          } else if (position == "stack") {
            myhist <- stackFreq(myhist, myGroup)
            base <- ggplot(data = myhist, aes(fill = myhist$myFill)) +
              geom_rect(
                mapping = aes(
                  xmin = start,
                  xmax = end,
                  ymin = ystart,
                  ymax = yend
                ),
                col = "black",
                alpha = alpha
              ) +
              theme(axis.text.x = element_text(
                angle = 90,
                vjust = 0.5,
                hjust = 1
              )) +
              labs(x = attr,
                   fill = "Group",
                   y = "Frequency") +
              scale_x_continuous(breaks = (myhist[myhist$myFill == myGroup[1], "start"] + myhist[myhist$myFill == myGroup[1], "end"]) /
                                   2,
                                 labels = unique(nameList)) +
              guides(colour = "none", alpha = "none")
          } else{
            stop("The position is not implemented.")
          }
          #make table
          for (i in unique(myhist$myFill)) {
            this.myhist <- dplyr::filter(myhist, myFill == i)
            myName <- paste("Table ", paste(unique(this.myhist$group), i, sep = ": "))
            resultSet[[myName]] <- data.frame(
              "Interval" = unique(nameList),
              "Observed Frequency" = this.myhist$frequency,
              "Relative Frequency" = this.myhist$frequency /
                sum(this.myhist$frequency)
            )
          }
          
        } else{
          base <- ggplot(data = myhist, aes(x = factor(interval), y = frequency)) +
            do.call(geom_bar, allmapping) +
            theme(axis.text.x = element_text(
              angle = 90,
              vjust = 0.5,
              hjust = 1
            )) +
            labs(x = attr) +
            scale_x_discrete(labels = nameList) +
            scale_fill_manual(values = rep("black", bins * length(attr))) +
            guides(colour = "none",
                   alpha = "none",
                   fill = "none")
          
          #make table
          resultSet[[paste0("Table ", unique(myhist$group))]] <- data.frame(
            "Interval" = nameList,
            "Observed Frequency" = myhist$frequency,
            "Relative Frequency" = myhist$frequency /
              sum(myhist$frequency)
          )
          
        }
      }
      
      resultSet[["plot"]] <- base
      return(as_ggInterval_hist_result(resultSet))
      
    } else{
      stop(paste0("ERROR : Unrecognize method : ", method, " ."))
    }
  })
}

#' @exportS3Method
print.ggInterval_hist_result <- function(x, ...) {
  if (!is.null(x$plot) && inherits(x$plot, "ggplot")) {
    print(x$plot)
    return(invisible(x))
  }
  print(unclass(x))
  invisible(x)
}

as_ggInterval_hist_result <- function(x) {
  class(x) <- c("ggInterval_hist_result", "list")
  x
}


stackFreq <- function(data = NULL, myGroup = NULL) {
  n <- dim(data)[1] / length(unique(data$group)) / length(myGroup)
  final <- NULL
  for (var in unique(data$group)) {
    work.data <- dplyr::filter(data, data$group == var)
    times <- n
    temp <- 0
    for (i in (length(myGroup) - 1):1) {
      temp <- temp + c(rep(0, times), work.data[1:(i * n), "frequency"])
      times <- times + n
    }
    final <- c(final, temp)
  }
  
  data[, "ystart"] <- final
  data[, "yend"] <- final + data$frequency
  return(data)
}
