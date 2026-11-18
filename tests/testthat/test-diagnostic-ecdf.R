test_that("endpoint envelopes and midpoint ECDF match a hand example", {
  x <- list(lower = cbind(X = c(0, 2)), upper = cbind(X = c(2, 4)))
  before <- x
  p <- ggInterval_ecdf(x)
  d <- attr(p, "interval_diagnostics")
  expect_s3_class(p, "ggplot")
  expect_identical(x, before)
  expect_equal(d$knots$x, c(0, 2, 4))
  expect_equal(d$knots$lower_cdf, c(0, 0.5, 1))
  expect_equal(d$knots$upper_cdf, c(0.5, 1, 1))
  expect_equal(d$midpoint$x, c(1, 3))
  expect_equal(d$midpoint$cdf, c(0.5, 1))
  interior <- d$band[d$band$xmin %in% c(0, 2), ]
  expect_equal(interior$xmax, c(2, 4))
  expect_equal(interior$lower_cdf, c(0, 0.5))
  expect_equal(interior$upper_cdf, c(0.5, 1))
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("envelopes bound empirical CDFs of admissible interval selections", {
  x <- list(lower = cbind(X = c(0, 1, 5)),
            upper = cbind(X = c(2, 4, 5)))
  d <- attr(ggInterval_ecdf(x), "interval_diagnostics")
  expect_equal(d$knots$x, c(0, 1, 2, 4, 5))
  expect_true(all(diff(d$knots$lower_cdf) >= 0))
  expect_true(all(diff(d$knots$upper_cdf) >= 0))
  expect_true(all(d$knots$lower_cdf <= d$knots$upper_cdf))
  expect_true(all(d$knots$lower_cdf >= 0 & d$knots$upper_cdf <= 1))
  for (chosen in list(c(0, 1, 5), c(2, 4, 5), c(0.5, 3, 5))) {
    f <- vapply(d$knots$x, function(t) mean(chosen <= t), numeric(1))
    expect_true(all(d$knots$lower_cdf <= f & f <= d$knots$upper_cdf))
  }
})

test_that("point intervals collapse the envelope, including a constant sample", {
  points <- c(-2, 0, 0, 4)
  x <- list(lower = cbind(X = points), upper = cbind(X = points))
  d <- attr(ggInterval_ecdf(x), "interval_diagnostics")
  expect_equal(d$knots$lower_cdf, c(0.25, 0.75, 1))
  expect_equal(d$knots$lower_cdf, d$knots$upper_cdf)
  expect_equal(d$band$lower_cdf, d$band$upper_cdf)
  constant <- list(lower = matrix(7, 3, 1), upper = matrix(7, 3, 1))
  p <- ggInterval_ecdf(constant)
  dc <- attr(p, "interval_diagnostics")
  expect_equal(dc$knots$x, 7)
  expect_equal(dc$knots$lower_cdf, 1)
  expect_equal(dc$band$lower_cdf, dc$band$upper_cdf)
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("selection order and midpoint omission are respected", {
  x <- list(lower = cbind(A = c(0, 1), B = c(10, 12)),
            upper = cbind(A = c(2, 3), B = c(11, 14)))
  p <- ggInterval_ecdf(x, variables = c("B", "A"), midpoint = FALSE)
  d <- attr(p, "interval_diagnostics")
  expect_identical(unique(d$knots$variable), c("B", "A"))
  expect_identical(levels(d$band$variable), c("B", "A"))
  expect_null(d$midpoint)
  expect_false(d$settings$midpoint)
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("midpoint must be an unambiguous logical scalar", {
  x <- list(lower = matrix(0, 1, 1), upper = matrix(1, 1, 1))
  for (bad in list(NA, c(TRUE, FALSE), 1, "yes", NULL)) {
    expect_error(ggInterval_ecdf(x, midpoint = bad), "midpoint.*TRUE or FALSE")
  }
})
