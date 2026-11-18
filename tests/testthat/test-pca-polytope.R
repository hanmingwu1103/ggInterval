# Polytope display of ggInterval_PCA: edge table and the requirement of complete vertex sets.
polytope_reference <- function(data) {
  lower <- sapply(data, min)
  upper <- sapply(data, max)
  n <- nrow(lower)
  p <- ncol(lower)
  corners <- as.matrix(expand.grid(rep(list(c(FALSE, TRUE)), p)))
  vertices <- do.call(rbind, lapply(seq_len(n), function(i) {
    t(apply(corners, 1, function(use_upper) ifelse(use_upper, upper[i, ], lower[i, ])))
  }))
  owner <- rep(seq_len(n), each = nrow(corners))
  standardized <- apply(vertices, 2, scale)
  fit <- stats::princomp(standardized)
  scores <- fit$scores[, 1:2, drop = FALSE]
  scores[, 1] <- -scores[, 1]
  edges <- NULL
  for (i in seq_len(n)) {
    rows <- which(owner == i)
    for (a in seq_along(rows)) {
      for (b in seq_along(rows)) {
        if (a < b && sum(corners[a, ] != corners[b, ]) == 1L) {
          edges <- rbind(edges, c(scores[rows[a], ], scores[rows[b], ]))
        }
      }
    }
  }
  edges
}

sorted_segments <- function(segments) {
  # an edge is unordered: put its two end points in a canonical order, then sort the edges
  segments <- unname(as.matrix(segments))
  swap <- segments[, 1] > segments[, 3] |
    (segments[, 1] == segments[, 3] & segments[, 2] > segments[, 4])
  segments[swap, ] <- segments[swap, c(3, 4, 1, 2)]
  segments[do.call(order, as.data.frame(round(segments, 8))), , drop = FALSE]
}

test_that("polytope edges equal the projected hyperrectangle edges", {
  result <- ggInterval_PCA(oils, poly = TRUE, plot = FALSE)
  drawn <- result$ggplotPCA$data[, c("x1", "y1", "x2", "y2")]
  reference <- polytope_reference(oils)
  expect_equal(nrow(drawn), nrow(oils) * ncol(oils) * 2^(ncol(oils) - 1))
  expect_identical(dim(as.matrix(drawn)), dim(reference))
  expect_equal(sorted_segments(drawn), sorted_segments(reference), tolerance = 1e-8)
})

test_that("polytope display reports incomplete vertex sets clearly", {
  expect_error(
    ggInterval_PCA(mushroom, poly = TRUE, plot = FALSE),
    "needs all 2\\^p distinct vertices"
  )
  expect_s3_class(ggInterval_PCA(mushroom, poly = FALSE, plot = FALSE), "princomp")
})
