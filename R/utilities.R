#' Generic function for the correlation
#' @name cor
#' @aliases cor
#' @author Oldemar Rodriguez Rojas
#' @description This function compute the symbolic correlation
#' @param x First symbolic variables.
#' @param y Second symbolic variables.
#' @param use an optional character string giving a method for computing
#' correlation in the presence of missing values. This must be (an abbreviation of)
#'  one of the strings 'everything', 'all.obs', 'complete.obs', 'na.or.complete',
#'  or 'pairwise.complete.obs'.
#' @param method The method to be use.
#' @param ... As in R cor function.
#'
#' @return Return a real number.
#' @references
#' Billard L. and  Diday E. (2006).
#' Symbolic data analysis: Conceptual statistics and data mining. Wiley, Chichester.
#'
#' Rodriguez, O. (2000).
#' Classification et Modeles Lineaires en Analyse des Donnees Symboliques. Ph.D. Thesis,
#' Paris IX-Dauphine University.
#'
#' @keywords Symbolic correlation
#' @export
cor <- function(x, ...) {
  UseMethod("cor", x)
}

#' @rdname cor
#' @export
cor.default <- function(x,
                        y = NULL,
                        use = "everything",
                        method = c("pearson", "kendall", "spearman"),
                        ...) {
  stats::cor(x, y, use, method)
}

#' @rdname cor
#' @export
cor.symbolic_tbl <- function(x, ...) {
  iData <- x
  
  isnumericData <- unlist(lapply(data.frame(iData[1:dim(iData)[2]]) , FUN = is.sym.interval))
  numericData <- data.frame(iData[, which(isnumericData)])
  p <- ncol(numericData)
  
  d <- sapply(1:p, function(a)
    sapply(1:p, function(b)
      cor(numericData[[a]], numericData[[b]], ...)))
  d <- as.data.frame(d)
  colnames(d) <- colnames(iData[, which(isnumericData)])
  rownames(d) <- colnames(iData[, which(isnumericData)])
  return(d)
  
}

#' @rdname cor
#' @export
cor.symbolic_interval <- function(x,
                                  y,
                                  method = c("centers", "B", "BD", "BG"),
                                  ...) {
  if (method == "centers") {
    out <- stats::cor((min(x) + max(x)) / 2, (min(y) + max(y)) / 2)
  } else{
    out <- cov(x, y, method, ...) / (RSDA::sd(x) * RSDA::sd(y))
  }
  return(out)
}



#' Generic function for the covariance
#' @name cov
#' @aliases cov
#' @author Oldemar Rodriguez Rojas
#' @description This function compute the symbolic covariance.
#' @param x First symbolic variables.
#' @param y Second symbolic variables.
#' @param use an optional character string giving a method for computing
#' covariances in the presence of missing values. This must be (an abbreviation of)
#'  one of the strings 'everything', 'all.obs', 'complete.obs', 'na.or.complete',
#'  or 'pairwise.complete.obs'.
#' @param method The method to be use.
#' @param na.rm As in R cov function.
#' @param ... As in R cov function.
#'
#' @return Return a real number.
#' @references
#' Billard L. and  Diday E. (2006).
#' Symbolic data analysis: Conceptual statistics and data mining. Wiley, Chichester.
#'
#' Rodriguez, O. (2000).
#' Classification et Modeles Lineaires en Analyse des Donnees Symboliques. Ph.D. Thesis,
#' Paris IX-Dauphine University.
#'
#' @keywords Symbolic Covariance
#' @export
cov <- function(x, ...) {
  UseMethod("cov", x)
}

#' @rdname cov
#' @export
cov.default <- function(x,
                        y = NULL,
                        use = "everything",
                        method = c("pearson", "kendall", "spearman"),
                        ...) {
  stats::cov(x, y, use, method)
}

#' @rdname cov
#' @export
cov.symbolic_tbl <- function(x, ...) {
  iData <- x
  
  isnumericData <- unlist(lapply(data.frame(iData[1:dim(iData)[2]]) , FUN = is.sym.interval))
  numericData <- data.frame(iData[, which(isnumericData)])
  p <- ncol(numericData)
  
  d <- sapply(1:p, function(a)
    sapply(1:p, function(b)
      cov(numericData[[a]], numericData[[b]], ...)))
  d <- as.data.frame(d)
  colnames(d) <- colnames(iData[, which(isnumericData)])
  rownames(d) <- colnames(iData[, which(isnumericData)])
  return(d)
  
}

#' @rdname cov
#' @export
cov.symbolic_interval <- function(x,
                                  y = NULL,
                                  method = c("centers", "B", "BD", "BG"),
                                  na.rm = FALSE,
                                  ...) {
  m <- length(x)
  Gj <- function(a, b, vmean) {
    if ((a + b) / 2 <= vmean) {
      return(-1)
    } else {
      return(1)
    }
  }
  Qj <- function(a, b, vmean) {
    return((a - vmean)^2 + (a - vmean) * (b - vmean) + (b - vmean)^2)
  }
  method <- match.arg(method)
  if (method == "centers") {
    out <- cov((min(x) + max(x)) / 2, (min(y) + max(y)) / 2)
    return(out)
  }
  if (method == "BD") {
    ss <- 0
    vmean.x <- mean(x, method = "centers")
    vmean.y <- mean(y, method = "centers")
    
    for (i in seq_len(length(x))) {
      ss <- ss + Gj(min(x[i]), max(x[i]), vmean.x) * Gj(min(y[i]), max(y[i]), vmean.y) * sqrt(Qj(min(x[i]), max(x[i]), vmean.x) *
                                                                                                Qj(min(y[i]), max(y[i]), vmean.y))
    }
    return((1 / (3 * length(x))) * ss)
  }
  if (method == "B") {
    a <- sum((min(x) + max(x)) * (min(y) + max(y))) / (4 * m)
    b <- (sum((min(x) + max(x))) * sum((min(y) + max(y)))) / (4 * m^2)
    return(a - b)
  }
  if (method == "BG") {
    x_bar <- mean(x)
    y_bar <- mean(y)
    a <- 2 * (min(x) - x_bar) * (min(y) - y_bar)
    b <- (min(x) - x_bar) * (max(y) - y_bar)
    c <- (max(x) - x_bar) * (min(y) - y_bar)
    d <- 2 * (max(x) - x_bar) * (max(y) - y_bar)
    return(sum(a + b + c + d) / (6 * m))
  }
}



#' @name RSDA2sym
#' @title RSDA object to symbolic object for ggplot
#' @description  It will be a good way to unify all symbolic data
#' object in R that collects all useful symbolic analysis tools
#' such like RSDA into the same class for management. In this way,
#' user who wants to do some study in symbolic data will be more
#' convenient for searching packages.Thus,RSDA2sym collecting RSDA
#' object into ggInterval object will do for plot(ggplot) and
#' RSDA's analysis.
#' @import stats
#' @param data an interval data, which may transfrom by RSDA::classic.to.sym
#' .Note:data is a necessary parameter,and must have symbolic_tbl class.
#' @param rawData rawData, which can be transformed to interval data,
#' must be a data frame and match to data.
#' @return Return an object of class "ggInterval", which
#' have a interval data and others as follows.
#' \itemize{
#'   \item intervalData - The Interval data after converting also known
#'   as a RSDA object.
#'   \item rawData - Classical data that user input.
#'   \item clusterResult - Cluster results .If the groupby method is
#'   a clustering method then it will exist.
#'   \item statisticsDF - A list contains data frame including some
#'   typically statistics in each group.
#' }#'
#' @usage RSDA2sym(data=NULL,rawData=NULL)
#'
#' @examples
#' r<-ggInterval::Cardiological
#' mySym<-RSDA2sym(r)
#' mySym$intervalData
#'
#' @export
RSDA2sym <- function(data = NULL, rawData = NULL) {
  if (!("symbolic_tbl" %in% class(data))) {
    stop("data must be a RSDA object. Missing \"symbolic_tbl\"")
  }
  pkg.env$intervalData <- data
  #if having row data
  if ((!is.null(rawData))) {
    if (!is.data.frame(rawData)) {
      stop("rawData must be a data frame")
    }
    if (!isMatch(data, rawData)) {
      stop("Cannot match data and rawData")
    }
  }
  numericData <- unlist(lapply(as.data.frame(data[, 1:dim(data)[2]]) , FUN = RSDA::is.sym.interval))
  numericData <- data[, numericData]
  
  pkg.env$statisticsDF <- buildStatsDf(numericData)
  names(pkg.env$statisticsDF) <- c("min", "median", "max")
  
  symObj <- ggInterval$new(
    rawData = rawData,
    statisticsDF = pkg.env$statisticsDF,
    intervalData = pkg.env$intervalData,
    clusterResult = pkg.env$result
  )
  return(symObj)
}

isMatch <- function(D, rowD) {
  a <- dim(rowD)[2] >= dim(D)[2]
  b <- colnames(D) %in% colnames(rowD)
  if (a && b) {
    return(TRUE)
  }
  return(FALSE)
}
pkg.env <- new.env()
pkg.env$statistics <- c("min", "median", "max", "mean")
pkg.env$statisticsDF <- NULL
pkg.env$result <- NULL
pkg.env$intervalData <- NULL
pkg.env$rawData <- NULL



#' @name scale
#' @aliases scale
#' @title scale for symbolic data table
#' @description scale for symbolic data table
#' @importFrom RSDA is.sym.interval
#' @param x A ggInterval object. It can also be either RSDA object or
#' classical data frame, which will be automatically convert to ggInterval
#' data.
#' @param center same as base::scale, either a logical value or numeric-alike vector of length equal to the number of columns of x, where nmeric-alike means that as.numeric(.) will be applied successfully if is.numeric(.) is not true.
#' @param scale same as base::scale, either a logical value or a numeric-alike vector of length equal to the number of columns of x.
#' @param ... Used by other R function.
#' @return Return a scale ggInterval object.
#' @examples
#'
#' #For all interval-valued
#' scale(facedata)
#'
#' #For both interval-valued and modal multi-valued
#' scale(mtcars.i)
#'
#' @keywords Symbolic scale
#' @export
scale <- function(x, ...) {
  UseMethod("scale")
}

#' @rdname scale
#' @export
scale.default <- function(x,
                          center = TRUE,
                          scale = TRUE,
                          ...) {
  base::scale.default(x, center, scale)
}

#' @rdname scale
#' @export
scale.symbolic_tbl <- function(x, ...) {
  #test data illegal
  ggSymData <- testData(x)
  iData <- ggSymData$intervalData
  
  
  #get interval-valued col
  interval.index <- lapply(
    1:ncol(iData),
    FUN = function(x)
      RSDA::is.sym.interval(iData[[x]])
  )
  none.interval.index <- which(!unlist(interval.index))
  interval.index <- which(unlist(interval.index))
  if (length(interval.index) == 0)
    stop("Cannot find interval-valued variables to scale.")
  n <- dim(iData)[1]
  
  #scale
  temp1 <- sapply(
    interval.index,
    FUN = function(x)
      unlist(data.frame(iData[[x]]))
  )
  temp2 <- apply(temp1, 2, scale)
  newd <- data.frame(temp2[1:n, ], temp2[(n + 1):(n * 2), ])
  myd <- classic2sym(newd,
                     groupby = "customize",
                     minData = temp2[1:n, ],
                     maxData = temp2[(n + 1):(n * 2), ])
  
  #initial result & merge interval-valued and modal
  result <- data.frame(matrix(NA, nrow = nrow(iData), ncol = ncol(iData)))
  result[, interval.index] <- myd$intervalData
  if (length(none.interval.index) != 0)
    result[, none.interval.index] <- iData[, none.interval.index]
  myd$intervalData <- result
  colnames(myd$intervalData) <- colnames(iData)
  rownames(myd$intervalData) <- rownames(iData)
  
  if (!("symbolic_tbl" %in% class(myd$intervalData))) {
    class(myd$intervalData) <- c(class(myd$intervalData), "symbolic_tbl")
  }
  
  return(myd)
}

#' @rdname scale
#' @export
scale.symbolic_interval <- function(x, ...) {
  s <- RSDA::sd(x)
  m <- mean(x)
  m1 <- (min(x) - m) / s
  m2 <- (max(x) - m) / s
  d <- data.frame(m1 = m1, m2 = m2)
  d2 <- classic2sym(d,
                    groupby = "customize",
                    minData = d$m1,
                    maxData = d$m2)
  
  return(d2$intervalData$V1)
}



#' @name summary
#' @aliases summary
#' @title summary for symbolic data table
#' @description summary for symbolic data table
#' @param object an object for which a summary is desired.
#' @param summary_fun only works when the symbolic_modal class input, it determine which summary function be applied for each modal.
#' @param ... additional arguments affecting the summary produced.
#' @return Return a summary table.
#' @examples
#'
#' #For all interval-valued
#' summary(facedata)
#'
#' #For both interval-valued and modal multi-valued
#' summary(Environment)
#'
#' summary(Environment$URBANICITY, summary_fun = "quantile")
#'
#'
#' @keywords Symbolic summary
#' @export
summary <- function(object, ...) {
  UseMethod("summary")
}

#' @rdname summary
#' @export
summary.default <- function(object, ...) {
  tryCatch({
    eval(parse(text = paste0("base::summary.", class(object)[1])))(object, ...)
  }, error = function(err1) {
    tryCatch({
      base::summary(object, ...)
    }, error = function(err2) {
      base::summary.default(object, ...)
    })
  })
}

#' @rdname summary
#' @export
summary.symbolic_tbl <- function(object, ...) {
  pkg.env$inPackage <- TRUE
  symbolic_interval <- NULL
  symbolic_modal <- NULL
  iData.boolean <- unlist(lapply(object, RSDA::is.sym.interval))
  mData.boolean <- unlist(lapply(object, RSDA::is.sym.modal))
  if (!all(iData.boolean | mData.boolean)) {
    stop(
      "Non-symbolic object detected. Please use classic2sym() to transform data into symbolic_tbl."
    )
  }
  result <- list(symbolic_interval = NULL, symbolic_modal = NULL)
  
  #For interval-valued data
  iData_ind <- which(iData.boolean)
  if (length(iData_ind) > 0) {
    tmp <- data.frame(matrix(0, nrow = 7, ncol = 1))#7: summary interval data will return 7 measures
    for (i in iData_ind) {
      tmp <- cbind(tmp, summary.symbolic_interval(object[, i][[1]]))
    }
    tmp <- tmp[, -1]
    if (class(tmp)[1] == "symbolic_interval") {
      tmp <- data.frame(dplyr::tibble(tmp))
      rownames(tmp) <- c("Min.",
                         "1st Qu.",
                         "Median",
                         "Mean",
                         "3rd Qu.",
                         "Max.",
                         "Std.")
    }
    colnames(tmp) <- colnames(object)[iData_ind]
    result$symbolic_interval <- tmp
  } else{
    result <- within(result, rm(symbolic_interval))
  }
  
  #For modal-multi valued data
  mData_ind <- which(mData.boolean)
  if (length(mData_ind) > 0) {
    tmp <- list(NULL)
    for (i in 1:length(mData_ind)) {
      tmp[[i]] <- summary.symbolic_modal(object[, mData_ind[i]][[1]], ...)
    }
    myMax <- max(unlist(lapply(tmp, length)))
    myMat <- matrix("", ncol = length(mData_ind), nrow = myMax)
    
    for (i in 1:length(tmp)) {
      myMat[1:nrow(tmp[[i]]), i] <- tmp[[i]]
    }
    colnames(myMat) <- colnames(object)[mData_ind]
    result$symbolic_modal <- noquote(myMat)
  } else{
    result <- within(result, rm(symbolic_modal))
  }
  pkg.env$inPackage <- FALSE
  return(result)
  
}

#' @rdname summary
#' @export
summary.symbolic_interval <- function(object, ...) {
  result <- NULL
  x <- object
  m1 <- min(x)
  m2 <- max(x)
  d <- data.frame(
    min_ = c(quantile(m1), mean(x), RSDA::sd(x)),
    max_ = c(quantile(m2), mean(x), RSDA::sd(x))
  )
  
  d2 <- classic2sym(
    d,
    groupby = "customize",
    minData = d$min_,
    maxData = d$max_
  )
  myNames <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.", "Std.")
  if (pkg.env$inPackage) {
    result <- as.data.frame(d2$intervalData[c(1:3, 6, 4:5, 7), ])
    rownames(result) <- myNames
    class(result) <- c("symbolic_tbl", class(result))
  } else{
    result <- c(d2$intervalData[c(1:3, 6, 4:5, 7), ])[[1]]
    names(result) <- myNames
  }
  
  return(result)
  #summary.symbolic_modal(object, ...) works
}

#' @rdname summary
#' @export
summary.symbolic_modal <- function(object, summary_fun = "mean", ...) {
  x <- object
  d <- data.frame(NULL)
  for (i in 1:length(x)) {
    d <- rbind(d, x[[i]]$prop)
  }
  colnames(d) <- x[[1]]$var
  result <- round(apply(d, 2, eval(parse(text = summary_fun))), 2)
  if (pkg.env$inPackage) {
    if (!is.null(dim(result))) {
      result <- round(apply(d, 2, mean, 2))
      warning("Dimension Error in summary_fun input. Autoadjust to mean summary.")
    }
    result <- paste(names(result), sprintf(result, fmt = "%.2f"), sep = ": ")
    result <- noquote(cbind(result))
  }
  #colnames(result) <- deparse(as.list(match.call())$object)
  return(result)
}
pkg.env <- new.env()
pkg.env$inPackage <- FALSE
