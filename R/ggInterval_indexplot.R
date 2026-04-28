#' @name ggInterval_indexplot
#' @title Plot the range of each observations
#' @description  Visualize the range of the variables of each observations
#' by using a kind of margin bar that indicate the minimal and maximal of
#' observations.
#' @import rlang ggplot2
#' @importFrom RSDA is.sym.interval
#' @param data A ggInterval object. It can also be either RSDA object or
#' classical data frame,which will be automatically convert to ggInterval
#' data.
#' @param mapping Set of aesthetic mappings created by aes() or aes_().
#' If specified and inherit. aes = TRUE (the default),
#' it is combined with the default mapping at the top level of
#' the plot. You must supply mapping if there is no plot mapping.
#' It is the same as the mapping of ggplot2.
#' @param plotAll plot all variables
#' @param row_order Row ordering used when \code{plotAll = TRUE}: \code{"o"}
#' for the original order, \code{"c"} for ascending interval centers within
#' each variable panel, \code{"r"} for ascending interval ranges within each
#' variable panel, and \code{"u"} for a user-defined permutation supplied
#' through \code{user_order}.
#' @param user_order User-defined row permutation used when
#' \code{row_order = "u"} and \code{plotAll = TRUE}. Supply either an integer
#' permutation of \code{1:nrow(data)} or a character permutation of the row
#' names.
#' @param labels Logical. When \code{TRUE} and \code{plotAll = TRUE}, repeat the
#' row labels inside each variable panel. This is especially useful for ordered
#' displays with \code{row_order = "c"} or \code{"r"}. For ordered displays,
#' the outer left-side row labels are suppressed by default.
#' @return Return a ggplot2 object.
#' @usage ggInterval_indexplot(data = NULL,mapping = aes(NULL),
#' plotAll = FALSE, row_order = "o", user_order = NULL, labels = FALSE)
#' @examples
#' mydata <- ggInterval::facedata
#' Subjects <- substr(rownames(mydata), 1, 3)
#' ggInterval_indexplot(mydata, aes(x = AD))
#' ggInterval_indexplot(mydata, aes(x = AD, fill = Subjects))
#' ggInterval_indexplot(mydata, plotAll = TRUE, row_order = "c")
#' custom_order <- c(19:21, 22:24, 1:18, 25:27)
#' ggInterval_indexplot(mydata["AD"], plotAll = TRUE,
#'                      row_order = "u", user_order = custom_order)
#' ggInterval_indexplot(mydata, plotAll = TRUE, row_order = "c", labels = TRUE)
#'
#' @export
ggInterval_indexplot <- function(data = NULL,
                             mapping = aes(NULL),
                             plotAll = FALSE,
                             row_order = "o",
                             user_order = NULL,
                             labels = FALSE) {
  row_order <- match.arg(row_order, c("o", "c", "r", "u"))
  if (!is.logical(labels) || length(labels) != 1 || is.na(labels)) {
    stop("labels must be either TRUE or FALSE.")
  }
  argsNum <- length(mapping)
  args <- lapply(mapping[1:argsNum], FUN = rlang::get_expr)
  this.x <- args$x
  this.y <- args$y
  this.fill <- args$fill
  this.group <- args$group
  
  ggSymData <- testData(data)#test if symdata
  iData <- ggSymData$intervalData
  if (plotAll) {
    if (!is.null(this.x) | !is.null(this.y)) {
      warning("Using plotAll presentation cannot specify variables.")
    }
  } else{
    if (row_order != "o") {
      warning("row_order is only used when plotAll = TRUE.")
    }
    if (!is.null(user_order)) {
      warning("user_order is only used when plotAll = TRUE and row_order = 'u'.")
    }
    if (labels) {
      warning("labels is only used when plotAll = TRUE.")
    }
    testXY(iData, this.x, this.y)
  }
  
  p <- dim(iData)[2]
  n <- dim(iData)[1]
  n.groups <- 1
  default_index_labels <- FALSE
  
  #adjust
  this.fill <- eval(this.fill)
  this.group <- eval(this.group)
  
  if (!is.null(this.group)) {
    if (length(this.group) == n | length(this.group) == 1) {
      #yes
      
      this.group <- as.factor(this.group)
      #test concepts_group illegal
      if (length(unique(table(this.group))) != 1) {
        stop("Each group of concepts must be equal length.")
      }
      
      if (length(this.group) == n) {
        n.concepts <- unique(table(this.group))
        n.groups <- length(table(this.group))
      }
    } else{
      #no
      
      stop(paste0("Length of fill must be equal to data (", n, ") or 1."))
    }
    
  }
  
  myRowNames <- rownames(iData)
  if (is.null(myRowNames)) {
    myRowNames <- as.character(seq_len(n))
  }
  if (plotAll && row_order != "u" && !is.null(user_order)) {
    warning("user_order is only used when row_order = 'u'.")
  }

  #test big o
  if (dim(iData)[1] >= 3000) {
    stop("Out of time limits.")
  } else if (dim(iData)[1] > 200 & dim(iData)[1] < 3000) {
    warning("It is not recommended for too many observations.")
  }
  
  isPlotX <- TRUE
  #temp<-c(which(names(args)=="x"),which(names(args)=="y"))
  #args.noXY<-args[-temp]
  with(data, {
    if (plotAll) {
      #isPlotX = FALSE is OK too
      isPlotX <- TRUE
      d <- NULL
      label_size <- if (n <= 20) {
        3
      } else if (n <= 35) {
        2.4
      } else if (n <= 60) {
        1.9
      } else{
        1.5
      }
      for (i in 1:p) {
        row_index <- reorder_plotall_rows(
          iData,
          row_order = row_order,
          user_order = user_order,
          row_names = myRowNames,
          variable = i
        )
        local_min <- iData[[i]]$min[row_index]
        local_max <- iData[[i]]$max[row_index]
        temp <- data.frame(
          myx = (local_min + local_max) / 2,
          myy = 1:n,
          min = local_min,
          max = local_max,
          g = colnames(iData)[[i]],
          row_label = myRowNames[row_index]
        )
        if (!is.null(this.fill)) {
          if (length(this.fill) == n) {
            temp$plot_fill <- this.fill[row_index]
          } else{
            temp$plot_fill <- rep(this.fill, n)
          }
        }
        if (!is.null(this.group)) {
          if (length(this.group) == n) {
            temp$plot_group <- this.group[row_index]
          } else{
            temp$plot_group <- rep(this.group, n)
          }
        }
        span <- max(temp$max, na.rm = TRUE) - min(temp$min, na.rm = TRUE)
        pad <- if (is.finite(span) && span > 0) {
          0.04 * span
        } else{
          0.5
        }
        temp$label_x <- temp$max + pad
        d <- rbind(d, temp)
      }
      d$g <- as.factor(d$g)
      
    } else{
      #autogenerate variable pretent user forget
      if (!is.null(this.group) & length(this.group) == n) {
        if (is.null(this.x)) {
          this.x <- rep(1:n.concepts, n.groups)
        } else if (is.null(this.y)) {
          this.y <- rep(1:n.concepts, n.groups)
        }
      } else{
        if (is.null(this.x)) {
          this.x <- 1:dim(iData)[1]
          default_index_labels <- TRUE
        } else if (is.null(this.y)) {
          this.y <- 1:dim(iData)[1]
          default_index_labels <- TRUE
        }
      }
      
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
        #adjust data number from user set
        if (!is.null(this.fill) & length(this.fill) == n) {
          #not change
          iData <- iData
        } else{
          iData <- iData[eval(this.y), ]
        }
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
        isPlotX <- FALSE
        if (!is.null(this.fill) & length(this.fill) == n) {
          #not change
          iData <- iData
        } else{
          iData <- iData[eval(this.x), ]
        }
      } else{
        stop("ERROR : Cannot find variables in aes(...)")
      }
      if (p == 1) {
        attr = colnames(data)
      }
      #test attribute illegal
      if (all(!is.numeric(data[[attr]]) , !RSDA::is.sym.interval(data[[attr]]))) {
        stop("ERROR : Variables in index Plot can only be numeric.")
      }
      
      mid <- (iData[[attr]]$min + iData[[attr]]$max) / 2
    }
    
    mymapping <- mapping
    
    #start plot
    if (plotAll) {
      mymapping$x <- d$myx
      mymapping$y <- d$myy
      if (!is.null(this.fill)) {
        mymapping$fill <- d$plot_fill
      }
      if (!is.null(this.group)) {
        mymapping$group <- d$plot_group
      }
      p <- plotAllFun(d, mymapping, this.fill, this.group,
                      labels = labels, label_size = label_size)
      if (row_order == "o") {
        return(p + scale_y_continuous(breaks = c(1:n), labels = myRowNames))
      }
      return(
        p +
          scale_y_continuous(breaks = c(1:n), labels = rep("", n)) +
          theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
      )
    }
    #deal with facets
    #iData <- addFactor(rawData = data, iData = iData)
    
    
    if (isPlotX) {
      mymapping$x <- mid
      mymapping$y <- this.y
      if (!is.null(this.group) & length(this.group) == n) {
        errorBar <- quote(geom_errorbar(
          aes(
            xmin = iData[[attr]]$min,
            xmax = iData[[attr]]$max,
            fill = this.group
          ),
          width = 0.2
        ))
        crossBar <- quote(geom_crossbar(
          aes(
            xmin = iData[[attr]]$min,
            xmax = iData[[attr]]$max,
            fill = this.group
          ),
          width = 0.5,
          fatten = 0.5
        ))
      } else{
        errorBar <- quote(geom_errorbar(aes(
          xmin = iData[[attr]]$min, xmax = iData[[attr]]$max
        ), width = 0.2))
        crossBar <- quote(geom_crossbar(aes(
          xmin = iData[[attr]]$min, xmax = iData[[attr]]$max
        ), width = 0.5, fatten = 0.5))
      }
      if (default_index_labels) {
        scale_xy <- scale_y_continuous(breaks = this.y, labels = myRowNames)
      } else {
        temp <- paste0("scale_y_continuous(breaks=c(", list(this.y), "))")
        scale_xy <- eval(parse(text = temp))
      }
      #temp2 <- paste0("scale_x_continuous(labels = myRowNames)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))")
      myLabs <- eval(parse(
        text = paste0(
          "labs(title='Index Plot',y='Observations',x='",
          attr,
          "')"
        )
      ))
    } else{
      mymapping$y <- mid
      mymapping$x <- this.x
      if (!is.null(this.group) & length(this.group) == n) {
        errorBar <- quote(geom_errorbar(
          aes(
            ymin = iData[[attr]]$min,
            ymax = iData[[attr]]$max,
            fill = this.group
          ),
          width = 0.2
        ))
        crossBar <- quote(geom_crossbar(
          aes(
            ymin = iData[[attr]]$min,
            ymax = iData[[attr]]$max,
            fill = this.group
          ),
          width = 0.5,
          fatten = 0.5
        ))
      } else{
        errorBar <- quote(geom_errorbar(aes(
          ymin = iData[[attr]]$min, ymax = iData[[attr]]$max
        ), width = 0.2))
        crossBar <- quote(geom_crossbar(aes(
          ymin = iData[[attr]]$min, ymax = iData[[attr]]$max
        ), width = 0.5, fatten = 0.5))
      }
      if (default_index_labels) {
        scale_xy <- scale_x_continuous(breaks = this.x, labels = myRowNames)
      } else {
        temp <- paste0("scale_x_continuous(breaks=c(", list(this.x), "))")
        scale_xy <- eval(parse(text = temp))
      }
      #temp2 <- paste0("scale_y_continuous(labels = myRowNames)")
      myLabs <- eval(parse(
        text = paste0(
          "labs(title='Index Plot',x='Observations',y='",
          attr,
          "')"
        )
      ))
    }
    mymapping$fill <- this.fill
    mymapping$group <- this.group
    p <- ggplot(iData, mapping = mymapping) +
      geom_point() +
      guides(alpha = "none") +
      scale_xy + myLabs
    if (!is.null(this.fill) | !is.null(this.group)) {
      p + eval(crossBar)
    } else{
      p + eval(errorBar)
    }
  })
}

reorder_plotall_rows <- function(iData,
                                 row_order = "o",
                                 user_order = NULL,
                                 row_names = rownames(iData),
                                 variable = NULL) {
  row_order <- match.arg(row_order, c("o", "c", "r", "u"))
  if (row_order == "o") {
    return(seq_len(nrow(iData)))
  }
  if (row_order == "u") {
    return(validate_user_order(user_order, nrow(iData), row_names))
  }
  if (is.null(variable) || length(variable) != 1) {
    stop("A single variable index must be supplied for row_order = 'c' or 'r'.")
  }
  row_summary <- switch(
    row_order,
    c = (iData[[variable]]$min + iData[[variable]]$max) / 2,
    r = iData[[variable]]$max - iData[[variable]]$min
  )
  order(row_summary)
}

validate_user_order <- function(user_order, n, row_names = NULL) {
  if (is.null(user_order)) {
    stop("user_order must be supplied when row_order = 'u'.")
  }
  if (is.factor(user_order)) {
    user_order <- as.character(user_order)
  }
  if (is.numeric(user_order)) {
    user_order_int <- as.integer(user_order)
    if (length(user_order_int) != n ||
        anyNA(user_order_int) ||
        !all(user_order == user_order_int) ||
        !setequal(user_order_int, seq_len(n))) {
      stop("user_order must be a permutation of 1:nrow(data).")
    }
    return(user_order_int)
  }
  if (is.character(user_order)) {
    if (is.null(row_names) || anyNA(row_names)) {
      stop("Character user_order requires non-missing row names.")
    }
    if (length(user_order) != n ||
        anyNA(user_order) ||
        anyDuplicated(user_order) ||
        !setequal(user_order, row_names)) {
      stop("Character user_order must be a permutation of the row names.")
    }
    return(match(user_order, row_names))
  }
  stop("user_order must be an integer or character permutation.")
}

plotAllFun <- function(d = NULL,
                       mymapping = NULL,
                       this.fill = NULL,
                       this.group = NULL,
                       labels = FALSE,
                       label_size = 2.4) {
  if (!is.null(this.fill) | !is.null(this.group)) {
    p <- ggplot(data = d, mapping = mymapping) +
      guides(alpha = "none") +
      geom_crossbar(aes(xmin = .data$min, xmax = .data$max),
                    width = 0.5, fatten = 0.5) +
      facet_grid(. ~ g, scales = "free")
  } else{
    p <- ggplot(data = d, mapping = mymapping) +
      guides(alpha = "none") +
      geom_errorbar(aes(xmin = .data$min, xmax = .data$max), width = 0.2) +
      facet_grid(. ~ g, scales = "free")
  }
  if (labels) {
    p <- p + geom_text(
      data = d,
      mapping = aes(x = .data$label_x, y = .data$myy, label = .data$row_label),
      inherit.aes = FALSE,
      hjust = 0,
      size = label_size,
      show.legend = FALSE
    )
  }
  p
}
