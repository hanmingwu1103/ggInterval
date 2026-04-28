make_test_intervals <- function() {
  classic2sym(
    data.frame(
      x_min = c(1, 2, 3, 4),
      x_max = c(3, 5, 6, 8),
      y_min = c(2, 1, 6, 7),
      y_max = c(5, 4, 8, 10)
    ),
    groupby = "customize",
    minData = data.frame(
      x = c(1, 2, 3, 4),
      y = c(2, 1, 6, 7)
    ),
    maxData = data.frame(
      x = c(3, 5, 6, 8),
      y = c(5, 4, 8, 10)
    )
  )$intervalData
}

cov_bg_expected <- function(x, y) {
  n <- length(x)
  sum((min(x) + max(x)) * (min(y) + max(y))) / (4 * n) -
    (sum(min(x) + max(x)) * sum(min(y) + max(y))) / (4 * n^2)
}

cov_bd_expected <- function(x, y) {
  n <- length(x)
  x_center_mean <- mean(x, method = "centers")
  y_center_mean <- mean(y, method = "centers")

  gj <- function(a, b, center_mean) {
    ifelse((a + b) / 2 <= center_mean, -1, 1)
  }

  qj <- function(a, b, center_mean) {
    (a - center_mean)^2 +
      (a - center_mean) * (b - center_mean) +
      (b - center_mean)^2
  }

  sum(
    gj(min(x), max(x), x_center_mean) *
      gj(min(y), max(y), y_center_mean) *
      sqrt(
        qj(min(x), max(x), x_center_mean) *
          qj(min(y), max(y), y_center_mean)
      )
  ) / (3 * n)
}

cov_b_expected <- function(x, y) {
  n <- length(x)
  x_bar <- mean(x)
  y_bar <- mean(y)

  a <- 2 * (min(x) - x_bar) * (min(y) - y_bar)
  b <- (min(x) - x_bar) * (max(y) - y_bar)
  c <- (max(x) - x_bar) * (min(y) - y_bar)
  d <- 2 * (max(x) - x_bar) * (max(y) - y_bar)

  sum(a + b + c + d) / (6 * n)
}

sd_billard_expected <- function(x) {
  sqrt((1 / (3 * length(x))) * sum(min(x)^2 + min(x) * max(x) + max(x)^2) -
         (1 / (4 * length(x)^2)) * sum(min(x) + max(x))^2)
}

test_that("symbolic covariance methods match their published formulas", {
  intervals <- make_test_intervals()
  x <- intervals[[1]]
  y <- intervals[[2]]

  expect_equal(cov(x, y, method = "centers"), stats::cov((min(x) + max(x)) / 2, (min(y) + max(y)) / 2))
  expect_equal(cov(x, y, method = "BD"), cov_bd_expected(x, y))
  expect_equal(cov(x, y, method = "BG"), cov_bg_expected(x, y))
  expect_equal(cov(x, y, method = "B"), cov_b_expected(x, y))
})

test_that("symbolic correlation methods use the matching covariance definitions", {
  intervals <- make_test_intervals()
  x <- intervals[[1]]
  y <- intervals[[2]]

  expect_equal(
    cor(x, y, method = "BD"),
    cov_bd_expected(x, y) / (sd_billard_expected(x) * sd_billard_expected(y))
  )
  expect_equal(
    cor(x, y, method = "BG"),
    cov_bg_expected(x, y) / sqrt(cov_bg_expected(x, x) * cov_bg_expected(y, y))
  )
  expect_equal(
    cor(x, y, method = "B"),
    cov_b_expected(x, y) / (sd_billard_expected(x) * sd_billard_expected(y))
  )
})

test_that("symbolic standard deviation defaults to the Billard formula", {
  intervals <- make_test_intervals()
  x <- intervals[[1]]

  expect_equal(sd(x), sd_billard_expected(x))
  expect_equal(sd(x, method = "B"), sd_billard_expected(x))
  expect_equal(sd(x, method = "BD"), sd_billard_expected(x))
  expect_equal(sd(x, method = "BG"), sqrt(cov_bg_expected(x, x)))
})

test_that("symbolic self-correlations are equal to one", {
  intervals <- make_test_intervals()
  x <- intervals[[1]]

  expect_equal(cor(x, x, method = "centers"), 1)
  expect_equal(cor(x, x, method = "B"), 1)
  expect_equal(cor(x, x, method = "BD"), 1)
  expect_equal(cor(x, x, method = "BG"), 1)
})

test_that("correlation heatmap values stay within [-1, 1] and keep a unit diagonal", {
  intervals <- make_test_intervals()
  plot_obj <- ggInterval_corrplot(intervals, method = "BG", triangle = "full")
  plot_data <- ggplot2::ggplot_build(plot_obj)$plot$data

  expect_true(all(plot_data$Correlation >= -1))
  expect_true(all(plot_data$Correlation <= 1))

  diag_data <- plot_data[plot_data$Var1 == plot_data$Var2, , drop = FALSE]
  expect_true(all(diag_data$Correlation == 1))
})

test_that("triangle selection matches the displayed lower and upper halves", {
  intervals <- make_test_intervals()

  lower_data <- ggplot2::ggplot_build(
    ggInterval_corrplot(intervals, method = "BG", triangle = "lower")
  )$plot$data
  upper_data <- ggplot2::ggplot_build(
    ggInterval_corrplot(intervals, method = "BG", triangle = "upper")
  )$plot$data

  expect_true(all(lower_data$i <= lower_data$j))
  expect_true(all(upper_data$i >= upper_data$j))
})
