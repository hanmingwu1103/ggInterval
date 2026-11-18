test_that("input validation preserves endpoint pairing and selection", {
  d <- list(lower = cbind(A = c(0, 1), B = c(-2, 0)),
            upper = cbind(A = c(1, 2), B = c(0, 3)))
  x <- ggInterval:::.ggInterval_endpoint_inputs(d, c("B", "A"))
  expect_identical(x$variables, c("B", "A"))
  expect_equal(unname(x$lower[, 1]), c(-2, 0))
  expect_identical(rownames(x$lower), x$ids)
  expect_error(ggInterval:::.ggInterval_endpoint_inputs(d, "C"), "existing")
  expect_error(ggInterval:::.ggInterval_endpoint_inputs(d, c("A", "A")), "unique")
  d$upper[1, 1] <- -1
  expect_error(ggInterval:::.ggInterval_endpoint_inputs(d), "lower endpoint")
  d$upper[1, 1] <- NA_real_
  expect_error(ggInterval:::.ggInterval_endpoint_inputs(d), "finite")
})

test_that("zero-width observations remain valid inputs", {
  d <- list(lower = matrix(c(1, 1), ncol = 1),
            upper = matrix(c(1, 1), ncol = 1))
  expect_equal(ggInterval:::.ggInterval_endpoint_inputs(d)$lower,
               ggInterval:::.ggInterval_endpoint_inputs(d)$upper)
})

test_that("ggInterval R6-style results are unwrapped without modification", {
  d <- list(lower = cbind(X = c(0, 1)), upper = cbind(X = c(2, 3)))
  wrapped <- new.env(parent = emptyenv())
  class(wrapped) <- c("ggInterval", "R6")
  wrapped$intervalData <- d
  before <- serialize(wrapped$intervalData, NULL)
  expect_identical(ggInterval:::.ggInterval_endpoint_inputs(wrapped),
                   ggInterval:::.ggInterval_endpoint_inputs(d))
  expect_identical(serialize(wrapped$intervalData, NULL), before)
  expect_error(ggInterval:::.ggInterval_endpoint_inputs(new.env()), "Unsupported")
})
