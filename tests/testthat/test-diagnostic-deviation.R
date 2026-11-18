deviation_fixture <- function() {
  lower <- cbind(X1 = c(0, 1, 4, 7), X2 = c(-2, 0, 4, 5))
  upper <- cbind(X1 = c(2, 5, 6, 11), X2 = c(0, 2, 8, 7))
  rownames(lower) <- rownames(upper) <- c("a", "b", "c", "d")
  list(lower = lower, upper = upper)
}

test_that("deviation components equal independently computed endpoint deviations", {
  d <- deviation_fixture()
  ref <- list(center = c(2, 1), range = c(3, 2))
  scales <- c(2, 4)
  plot <- ggInterval_deviation(d, scale = scales, reference = ref,
                               highlight = c(FALSE, TRUE, FALSE, FALSE))
  diagnostics <- attr(plot, "interval_diagnostics")
  a0 <- ref$center - ref$range / 2
  b0 <- ref$center + ref$range / 2
  expected <- rowSums((sweep(sweep(d$lower, 2, a0), 2, scales, "/")^2 +
                         sweep(sweep(d$upper, 2, b0), 2, scales, "/")^2) / 2)
  expect_equal(diagnostics$contributions$total, unname(expected))
  expect_equal(unname(diagnostics$endpoint_squared_deviation), unname(expected))
  expect_equal(unname(diagnostics$identity_error), rep(0, 4), tolerance = 1e-14)
  expect_equal(diagnostics$order, order(-expected, seq_along(expected)))
  expect_identical(diagnostics$contributions$highlighted, c(FALSE, TRUE, FALSE, FALSE))
  expect_s3_class(plot, "ggplot")
  built <- ggplot2::ggplot_build(plot)
  expect_equal(nrow(built$data[[1]]), 8L)
})

test_that("pooled and group medians are explicit and translation-scale invariant", {
  d <- deviation_fixture()
  pooled <- attr(ggInterval_deviation(d), "interval_diagnostics")
  grouped <- attr(ggInterval_deviation(d, group = c("A", "A", "B", "B")),
                  "interval_diagnostics")
  centers <- (d$lower + d$upper) / 2
  widths <- d$upper - d$lower
  expect_equal(unname(pooled$reference$center[1, ]), apply(centers, 2, median), ignore_attr = TRUE)
  expect_equal(unname(grouped$reference$center[1, ]), apply(centers[1:2, ], 2, median), ignore_attr = TRUE)
  expect_equal(unname(grouped$reference$range[4, ]), apply(widths[3:4, ], 2, median), ignore_attr = TRUE)
  expect_false(isTRUE(all.equal(pooled$contributions$total, grouped$contributions$total)))
  factor <- c(3, 0.2)
  shift <- c(10, -7)
  changed <- list(lower = sweep(sweep(d$lower, 2, factor, "*"), 2, shift, "+"),
                  upper = sweep(sweep(d$upper, 2, factor, "*"), 2, shift, "+"))
  changed_diag <- attr(ggInterval_deviation(changed, group = c("A", "A", "B", "B")),
                      "interval_diagnostics")
  expect_equal(changed_diag$contributions$total, grouped$contributions$total, tolerance = 1e-12)
  expect_equal(changed_diag$contributions$center, grouped$contributions$center, tolerance = 1e-12)
  expect_equal(changed_diag$contributions$range, grouped$contributions$range, tolerance = 1e-12)
})

test_that("constant coordinates are omitted only for default zero spans", {
  d <- deviation_fixture()
  d$lower <- cbind(d$lower, constant = 7)
  d$upper <- cbind(d$upper, constant = 7)
  diag <- attr(ggInterval_deviation(d), "interval_diagnostics")
  expect_identical(diag$omitted_variables, "constant")
  expect_equal(diag$contributions$total,
               attr(ggInterval_deviation(deviation_fixture()), "interval_diagnostics")$contributions$total)
  constant <- list(lower = matrix(7, 3, 1, dimnames = list(NULL, "C")),
                   upper = matrix(7, 3, 1, dimnames = list(NULL, "C")))
  all_constant <- ggInterval_deviation(constant)
  expect_equal(attr(all_constant, "interval_diagnostics")$contributions$total, c(0, 0, 0))
  expect_silent(ggplot2::ggplot_build(all_constant))
  expect_error(ggInterval_deviation(constant,
               reference = list(center = 6, range = 0)), "explicit positive.*scale")
  expect_error(ggInterval_deviation(constant,
               reference = list(center = 7, range = 2)), "explicit positive.*scale")
  explicit <- attr(ggInterval_deviation(constant, scale = 1,
                   reference = list(center = 6, range = 0)), "interval_diagnostics")
  expect_equal(explicit$contributions$total, c(1, 1, 1))
})

test_that("deviation rejects ambiguous or invalid supplied arguments", {
  d <- deviation_fixture()
  expect_error(ggInterval_deviation(d, scale = c(0, 1)), "positive finite")
  expect_error(ggInterval_deviation(d, scale = 1), "positive finite")
  expect_error(ggInterval_deviation(d, reference = list(center = c(0, 0), range = c(-1, 1))),
                "nonnegative full")
  expect_error(ggInterval_deviation(d, reference = list(center = c(0, Inf), range = c(1, 1))),
                "finite numeric")
  expect_error(ggInterval_deviation(d, highlight = 1:4), "logical vector")
  expect_error(ggInterval_deviation(d, labels = rep("same", 4)), "distinct")
  expect_error(ggInterval_deviation(d, group = c("A", NA, "B", "B")), "nonmissing")
})
