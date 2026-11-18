.crcor_test_input <- function(centers, ranges) {
  list(lower = centers - ranges / 2, upper = centers + ranges / 2)
}

test_that("pooled component correlations agree with hand calculations", {
  centers <- cbind(X = c(0, 1, 2, 3), Y = c(0, 1, 0, 1))
  ranges <- cbind(X = c(2, 4, 6, 8), Y = rep(2, 4))
  x <- .crcor_test_input(centers, ranges)
  before <- x
  p <- ggInterval_CRcor(x)
  d <- attr(p, "interval_diagnostics")
  expect_s3_class(p, "ggplot")
  expect_identical(x, before)
  expect_identical(colnames(d$values), c("C1", "C2", "R1", "R2"))
  expect_equal(unname(d$values[, 1:2]), unname(centers))
  expect_equal(unname(d$values[, 3:4]), unname(ranges))
  expect_equal(d$pooled["C1", "C2"], 1 / sqrt(5))
  expect_equal(d$pooled["C1", "R1"], 1)
  expect_true(all(is.na(d$pooled["R2", ])))
  expect_equal(d$displayed, d$pooled)
  expect_null(d$group_demeaned)
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("pooled and group-demeaned correlations occupy the correct triangles", {
  centers <- cbind(X = c(0, 1, 10, 11), Y = c(1, 0, 21, 20))
  ranges <- matrix(2, 4, 2, dimnames = list(NULL, c("X", "Y")))
  p <- ggInterval_CRcor(.crcor_test_input(centers, ranges),
                       group = c("a", "a", "b", "b"))
  d <- attr(p, "interval_diagnostics")
  pooled <- 199 / sqrt(101 * 401)
  expect_equal(d$pooled["C1", "C2"], pooled)
  expect_equal(d$group_demeaned["C1", "C2"], -1)
  expect_equal(d$displayed["C2", "C1"], pooled)
  expect_equal(d$displayed["C1", "C2"], -1)
  expect_equal(unname(d$residuals[, 1]), c(-0.5, 0.5, -0.5, 0.5))
  expect_equal(unname(d$residuals[, 2]), c(0.5, -0.5, 0.5, -0.5))
  tiles <- ggplot2::ggplot_build(p)$data[[1]]
  expect_equal(as.numeric(tiles$x), d$cells$column_index)
  expect_equal(as.numeric(tiles$y), 5 - d$cells$row_index)
})

test_that("a single group agrees with pooled Pearson correlations", {
  centers <- cbind(X = c(-2, 0, 3), Y = c(1, 4, 2))
  ranges <- cbind(X = c(1, 2, 3), Y = c(4, 1, 2))
  p <- ggInterval_CRcor(.crcor_test_input(centers, ranges), group = rep("g", 3))
  d <- attr(p, "interval_diagnostics")
  expect_equal(d$group_demeaned, d$pooled, tolerance = 1e-12)
})

test_that("singleton groups are disclosed and contribute zero residuals", {
  centers <- cbind(X = c(0, 1, 5), Y = c(1, 2, 0))
  ranges <- matrix(2, 3, 2, dimnames = list(NULL, c("X", "Y")))
  expect_warning(
    p <- ggInterval_CRcor(.crcor_test_input(centers, ranges),
                         group = c("repeat", "repeat", "single")),
    "Singleton groups contribute zero"
  )
  d <- attr(p, "interval_diagnostics")
  expect_equal(unname(d$residuals[3, ]), rep(0, 4))
  expect_equal(d$group_demeaned["C1", "C2"], 1)
})

test_that("undefined correlations remain NA and no residual information errors", {
  x <- .crcor_test_input(cbind(X = c(2, 2, 2)), cbind(X = c(1, 2, 3)))
  d <- attr(ggInterval_CRcor(x), "interval_diagnostics")
  expect_true(all(is.na(d$pooled["C1", ])))
  expect_equal(d$pooled["R1", "R1"], 1)
  expect_error(ggInterval_CRcor(x, group = c("a", "b", "c")),
               "No within-group variation")
  grouped_constant <- .crcor_test_input(cbind(X = c(0, 0, 1, 1)),
                                        cbind(X = rep(2, 4)))
  expect_error(ggInterval_CRcor(grouped_constant, group = c(1, 1, 2, 2)),
               "No within-group variation")
  single <- list(lower = matrix(0, 1, 1), upper = matrix(1, 1, 1))
  p <- ggInterval_CRcor(single)
  expect_true(all(is.na(attr(p, "interval_diagnostics")$pooled)))
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("group and label arguments are validated", {
  x <- list(lower = cbind(X = c(0, 1)), upper = cbind(X = c(1, 2)))
  for (bad in list(1, c(1, NA), list("a", "b"), matrix(c(1, 2), 2, 1))) {
    expect_error(ggInterval_CRcor(x, group = bad), "group.*vector of length n")
  }
  for (bad in list(NA, c(TRUE, FALSE), 1, "yes", NULL)) {
    expect_error(ggInterval_CRcor(x, labels = bad), "labels.*TRUE or FALSE")
  }
})

test_that("selected variables determine component keys in the requested order", {
  x <- list(lower = cbind(A = c(0, 1), B = c(2, 4)),
            upper = cbind(A = c(1, 3), B = c(4, 7)))
  d <- attr(ggInterval_CRcor(x, variables = c("B", "A"), labels = FALSE),
            "interval_diagnostics")
  expect_identical(d$variables$variable, c("B", "A", "B", "A"))
  expect_identical(d$variables$component, c("center", "center", "full range", "full range"))
  expect_false(d$settings$labels)
})
