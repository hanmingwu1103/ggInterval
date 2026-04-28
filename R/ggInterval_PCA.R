#' @name ggInterval_PCA
#' @title Vertices PCA for interval data
#' @description ggInterval_PCA performs a principal components
#' analysis on the given numeric interval data and returns the
#' results of princomp, a ggplot object, and interval scores.
#' @import rlang ggplot2 stats
#' @importFrom RSDA is.sym.interval
#' @importFrom gtools odd
#' @param data A ggInterval object. It can also be either RSDA object or
#' classical data frame, which will be automatically convert to ggInterval
#' data.
#' @param mapping  Set of aesthetic mappings created by aes() or aes_().
#' If specified and inherit. aes = TRUE (the default),
#' it is combined with the default mapping at the top level of
#' the plot. You must supply mapping if there is no plot mapping.
#' It is the same as the mapping of ggplot2.
#' @param plot Boolean variable,Auto plot (if TRUE).It can also plot by
#' its inner object
#' @param concepts_group color with each group of concept
#' @param poly if plot a poly result
#' @param adjust adjust sign of the principal component
#' @param showLabels Logical. If \code{TRUE}, label interval scores in the plot.
#' @param labelSize Numeric text size used when \code{showLabels = TRUE}.
#' @param checkOverlap Logical. Passed to \code{geom_text()} to suppress
#' overlapping labels when needed.
#' @return A ggplot object for PC1, PC2, and interval scores with related outputs.
#' \itemize{
#'   \item scores_interval - The interval scores after PCA.
#'   \item ggplotPCA - a ggplot object with x-axis and y-axis are PC1 and
#'   PC2.
#'   \item others - others are the returns values of princomp.
#' }
#' @usage ggInterval_PCA(data = NULL,mapping = aes(NULL),plot=TRUE,
#'                       concepts_group=NULL, poly = FALSE, adjust = TRUE,
#'                       showLabels = TRUE, labelSize = 3,
#'                       checkOverlap = FALSE)
#' @examples
#' \donttest{
#' Subjects <- substr(rownames(facedata), 1, 3)
#' p <- ggInterval_PCA(facedata, plot = FALSE, concepts_group = Subjects)
#' p$ggplotPCA
#' p$scores_interval
#' ggInterval_PCA(facedata, poly = TRUE, concepts_group = Subjects)
#' }
#'
#' @export
ggInterval_PCA <- function(data = NULL,
                           mapping = aes(NULL),
                           plot = TRUE,
                           concepts_group = NULL,
                           poly = FALSE,
                           adjust = TRUE,
                           showLabels = TRUE,
                           labelSize = 3,
                           checkOverlap = FALSE) {
  #data preparing
  argsNum <- length(mapping)
  args <- lapply(mapping[1:argsNum], FUN = rlang::get_expr)
  temp <- c(which(names(args) == "x"), which(names(args) == "y"))
  if (length(temp) > 0) {
    args.noXY <- args[-temp]
  } else{
    args.noXY <- args
  }
  
  #test data illegal
  ggSymData <- testData(data)
  iData <- ggSymData$intervalData
  myRowNames <- rownames(iData)
  
  #preparing data
  temp.1 <- ggSymData$statisticsDF$min
  temp.2 <- ggSymData$statisticsDF$max
  n <- dim(temp.1)[1]
  p <- dim(temp.1)[2]
  if (!is.null(concepts_group)) {
    if (length(concepts_group) != n) {
      stop(paste0("Length of concepts group must be equal to data (", n, ")."))
    }
    concepts_group <- as.factor(concepts_group)
  }
  
  
  #test big o
  if (n > 2000) {
    stop("Out of time limits.")
  }
  
  if (p < 2) {
    stop("ERROR : Variables need at least 2.")
  }
  vertice <- cbind(get_vertice(p), 2 * p + 1)
  
  #build PCA data
  pcaData <- lapply(
    1:p,
    FUN = function(x)
      cbind(temp.1[, x], temp.2[, x])
  )
  pcaData <- data.frame(pcaData, c(1:n))
  pcaData <- as.matrix(pcaData)
  
  #Meger all pca data into a large matrix m
  m <- NULL
  for (i in 1:n) {
    temp <- t(sapply(
      1:2^p,
      FUN = function(x)
        rbind(NULL, pcaData[i, vertice[x, ]])
    ))
    temp <- cbind(temp, 1:2^p)
    m <- rbind(m, temp)
  }
  
  #remove repeat value
  m <- m[!duplicated(as.data.frame(m[, 1:p])), ]
  m[, 1:p] <- apply(m[, 1:p], 2, scale)
  
  #start PCA
  mypca <- stats::princomp(m[, 1:p])
  if (adjust) {
    mypca$scores[, 1] <- mypca$scores[, 1] * -1
    mypca$scores[, 3] <- mypca$scores[, 3] * -1
  }
  
  PCscores <- data.frame(mypca$scores, u = m[, p + 1])
  
  #plot Poly PCA
  #plot Dim.1 and Dim.2 cause no.comp : number of component = 2
  if (poly) {
    polyList <- build_PCA_poly_data(mypca$scores, m, n, p, no.comp = 2, group = concepts_group)
    d <- polyList$cpData
    usermapping <- structure(as.expression(args.noXY), class = "uneval") #Aesthetic without x,y
    mymapping <- list(
      d,
      mapping = aes(
        x = .data$x1,
        y = .data$y1,
        xend = .data$x2,
        yend = .data$y2,
        col = .data$Concepts_Group
      )
    )
    
    allmapping <- as.list(structure(as.expression(c(
      usermapping, mymapping
    )), class = "uneval"))
    
    
    base <- ggplot(d, aes(.data$x1, .data$y1)) +
      do.call(geom_segment, allmapping)
    
    
  }
  
  
  #build PCA interval data
  PCscores_interval <- matrix(0, nrow = n, ncol = 2 * p)
  for (obs in 1:n) {
    for (var in 1:p) {
      PCscores_interval[obs, 2 * var - 1] <- min(PCscores[PCscores$u == obs, var])
      PCscores_interval[obs, 2 * var] <- max(PCscores[PCscores$u == obs, var])
    }
  }
  PCscores_interval <- as.data.frame(PCscores_interval)
  
  
  myname <- NULL
  for (i in 1:p) {
    a <- paste0("PC", i, ".min")
    b <- paste0("PC", i, ".max")
    myname <- append(myname, append(a, b))
  }
  colnames(PCscores_interval) <- myname
  mypca$scores_interval <- adjustToRSDA(PCscores_interval)
  mypca$scores_interval[, "rowname"] <- rownames(iData)
  mypca$scores_interval <- tibble::column_to_rownames(mypca$scores_interval, var = "rowname")
  class(mypca$scores_interval) <- c(class(mypca$scores_interval), "symbolic_tbl")
  PCscores_interval$rowname_label <- myRowNames
  #蝑??ㄐ ?隞半uild rownames ????rownames??絲靘??
  #build Aesthetic (mapping)
  if (!poly) {
    usermapping <- structure(as.expression(args.noXY), class = "uneval") #Aesthetic without x,y
    
    if (!is.null(concepts_group)) {
      mymapping <- list(
        PCscores_interval,
        mapping = aes(
          xmin = .data$PC1.min,
          xmax = .data$PC1.max,
          ymin = .data$PC2.min,
          ymax = .data$PC2.max,
          fill = concepts_group,
          alpha = 0.5
        ),
        col = "black"
      )
    } else{
      mymapping <- list(
        PCscores_interval,
        mapping = aes(
          xmin = .data$PC1.min,
          xmax = .data$PC1.max,
          ymin = .data$PC2.min,
          ymax = .data$PC2.max,
          fill = grDevices::gray.colors(n),
          alpha = 0.5
        ),
        col = "black"
      )
    }
    allmapping <- as.list(structure(as.expression(c(
      usermapping, mymapping
    )), class = "uneval"))
    
    #plot
    base <- ggplot(PCscores_interval, aes(.data$PC1.min, .data$PC2.min)) +
      do.call(geom_rect, allmapping) +
      guides(colour = "none", alpha = "none")
    
    if (showLabels) {
      base <- base +
        geom_text(
          aes(label = .data$rowname_label),
          size = labelSize,
          check_overlap = checkOverlap
        )
    }
    
    if (is.null(concepts_group)) {
      base <- base + scale_fill_manual(name = "Concept",
                                       values = gray.colors(n),
                                       labels = myRowNames)
    }
  }
  
  if (is.null(concepts_group)) {
    base <- base + guides(fill = "none",
                          colour = "none")
  }
  base <- base + labs(x = "PC1", y = "PC2")
  mypca$ggplotPCA <- base
  if (plot) {
    plot(base)
  }
  return(mypca)
}

get_vertice <- function(p) {
  origin <- matrix(which(gtools::odd(1:(2 * p))), nrow = 1)
  for (i in p:1) {
    temp <- origin
    temp[, i] <- temp[, i] + 1
    origin <- rbind(origin, temp)
  }
  return(origin)
}
adjustToRSDA <- function(data) {
  size <- dim(data)[1] * dim(data)[2]
  p <- dim(data)[2] / 2
  tempd <- matrix(0, nrow = size / p, ncol = p)
  odd <- unlist(as.list(matrix(which(
    gtools::odd(1:nrow(tempd))
  ), nrow = 1)))
  even <- odd + 1
  for (i in 1:p) {
    tempd[odd, i] <- data[, i * 2 - 1]
    tempd[even, i] <- data[, i * 2]
  }
  tempd <- cbind(tempd, rep(1:(size / (p * 2)), each = 2))
  tempd <- as.data.frame(tempd)
  tempd <- RSDA::classic.to.sym(tempd, concept = colnames(tempd)[dim(tempd)[2]])
  colnames(tempd) <- paste0("PC_", 1:p)
  return(tempd)
}


build_PCA_poly_data <- function(scores, m, n, p, no.comp, group) {
  polyData <- scores[, 1:no.comp]
  cp <- connectPoint(m[, 1:p], n, p)
  Ni <- cp[length(cp)]
  num <- dim(cp)[1]
  cpData <- NULL
  if (is.null(group)) {
    group <- rep(1, n)
  }
  for (i in 1:num) {
    Hi <- scores[((cp[i, 1] - 1) * Ni + 1):(cp[i, 1] * Ni), ]
    cpData <- rbind(cpData,
                    data.frame(
                      x1 = Hi[cp[i, 2], 1],
                      y1 = Hi[cp[i, 2], 2],
                      x2 = Hi[cp[i, 3], 1],
                      y2 = Hi[cp[i, 3], 2],
                      Concepts_Group = group[cp[i, 1]]
                    ))
  }
  cpData$Concepts_Group <- as.factor(cpData$Concepts_Group)
  polyData <- as.data.frame(polyData)
  colnames(polyData) <- paste0("PC", 1:no.comp)
  return(list(polyData = polyData, cpData = cpData))
}


connectPoint <- function(vdata, m, p) {
  Ni <- 2^p
  tempMatrix <- matrix(0, nrow = m * Ni * p, ncol = 3)
  count <- 0
  for (k in 1:m) {
    Hi <- vdata[((k - 1) * Ni + 1):(k * Ni), ]
    for (i in 1:(Ni - 1)) {
      for (j in (i + 1):Ni) {
        if (length(which(Hi[i, ] - Hi[j, ] == 0)) == (p - 1)) {
          count = count + 1
          tempMatrix[count, 1] <- k
          tempMatrix[count, 2] <- i
          tempMatrix[count, 3] <- j
        }
        
      }
    }
    
  }
  Cmatrix <- tempMatrix[1:count, ]
  Cmatrix
  #Cmatrix[k i j],
  #k=1,..., m;
  #Hi: i=1,...,Ni;
  #j: the points that connect to i
  
}
