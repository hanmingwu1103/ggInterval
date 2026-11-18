overlap_fixture <- function() {
  lower <- rbind(A = c(0, 0), B = c(1, 1), C = c(4, 4), D = c(0.5, 0.5))
  upper <- rbind(A = c(2, 2), B = c(3, 3), C = c(5, 5), D = c(1.5, 1.5))
  colnames(lower) <- colnames(upper) <- c("X1", "X2")
  list(lower = lower, upper = upper)
}

test_that("box overlap matches hand calculations, including containment and disjointness", {
  p <- ggInterval_overlap(overlap_fixture())
  m <- attr(p, "interval_diagnostics")$overlap
  expect_equal(m, t(m))
  expect_equal(unname(diag(m)), rep(1, 4))
  expect_equal(m["A", "B"], 1 / 7)
  expect_equal(m["A", "C"], 0)
  expect_equal(m["A", "D"], 1 / 4)
  expect_equal(m["B", "D"], 0.25 / 4.75)
  expect_true(all(m >= 0 & m <= 1))
  expect_s3_class(p, "ggplot")
  expect_equal(nrow(ggplot2::ggplot_build(p)$data[[1]]), 16L)
})

test_that("overlap is invariant to affine transformations and row permutations", {
  d <- overlap_fixture()
  base <- attr(ggInterval_overlap(d), "interval_diagnostics")$overlap
  multiplier <- c(3, 0.125)
  shift <- c(-10, 4)
  changed <- list(lower = sweep(sweep(d$lower, 2, multiplier, "*"), 2, shift, "+"),
                  upper = sweep(sweep(d$upper, 2, multiplier, "*"), 2, shift, "+"))
  expect_equal(attr(ggInterval_overlap(changed), "interval_diagnostics")$overlap, base)
  perm <- c(3, 1, 4, 2)
  changed <- list(lower = d$lower[perm, , drop = FALSE], upper = d$upper[perm, , drop = FALSE])
  permuted <- attr(ggInterval_overlap(changed), "interval_diagnostics")$overlap
  expect_equal(permuted[rownames(base), colnames(base)], base)
  # Products 1e-200 ^ 20 underflow; the volume ratio remains well defined.
  tiny <- list(lower = matrix(0, 2, 20), upper = rbind(rep(1e-200, 20), rep(2e-200, 20)))
  expect_equal(attr(ggInterval_overlap(tiny), "interval_diagnostics")$overlap[1, 2],
               2^-20, tolerance = 1e-10)
})

test_that("group ordering is stable and factor levels are respected", {
  d <- overlap_fixture()
  g <- factor(c("second", "first", "second", "first"), levels = c("first", "second"))
  diag <- attr(ggInterval_overlap(d, group = g), "interval_diagnostics")
  expect_identical(diag$order, c(2L, 4L, 1L, 3L))
  expect_identical(diag$displayed_ids, c("B", "D", "A", "C"))
  expect_equal(diag$group_boundaries, 2L)
  chars <- attr(ggInterval_overlap(d, group = as.character(g)), "interval_diagnostics")
  expect_identical(chars$order, c(1L, 3L, 2L, 4L))
})

test_that("zero-volume boxes are explicitly undefined and invalid inputs fail", {
  d <- overlap_fixture()
  d$upper[1, 1] <- d$lower[1, 1]
  expect_error(ggInterval_overlap(d), "zero-volume")
  diag <- attr(ggInterval_overlap(d, zero_width = "na"), "interval_diagnostics")
  expect_true(all(is.na(diag$overlap[1, ])))
  expect_true(all(is.na(diag$overlap[, 1])))
  expect_equal(unname(diag(diag$overlap[-1, -1])), rep(1, 3))
  constant <- list(lower = matrix(1, 2, 1), upper = matrix(1, 2, 1))
  expect_error(ggInterval_overlap(constant), "zero-volume")
  p <- ggInterval_overlap(constant, zero_width = "na")
  expect_true(all(is.na(attr(p, "interval_diagnostics")$overlap)))
  expect_silent(ggplot2::ggplot_build(p))
  expect_error(ggInterval_overlap(d, zero_width = "zero"), "arg")
  expect_error(ggInterval_overlap(overlap_fixture(), labels = c("a", "a", "b", "c")), "distinct")
  expect_error(ggInterval_overlap(overlap_fixture(), group = "a"), "nonmissing")
})
