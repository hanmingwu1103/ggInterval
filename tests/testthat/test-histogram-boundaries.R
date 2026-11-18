# Regression and public-interface tests for histogram boundary handling (ggInterval 0.2.6).
# Mass, probability and density comparisons use tolerance 1e-8.

# classic2sym(groupby = "customize") names the converted columns V1, V2, ...;
# restore the intended names. ggInterval_hist()/ggInterval_2Dhist() locate aes()
# variables by value identity, so fixture columns must differ in values.
hist_fixture <- function(lower, upper) {
  d <- classic2sym(data.frame(lower, upper), groupby = "customize",
                   minData = lower, maxData = upper)$intervalData
  names(d) <- names(lower)
  d
}

hist_tables <- function(result) result[grepl("^Table", names(result))]

# Grouping vectors in aes(fill = ) are evaluated from the package frame, so a
# test-local vector is not visible; the grouped test publishes its vectors in
# the global environment and removes them with on.exit().

expect_hist_mass <- function(cells, n) {
  expect_true(all(is.finite(as.matrix(cells[, c("f", "p", "h", "area")]))))
  expect_true(all(cells$area > 0))
  expect_equal(sum(cells$f), n, tolerance = 1e-8)
  expect_equal(sum(cells$p), 1, tolerance = 1e-8)
  expect_equal(sum(cells$h * cells$area), 1, tolerance = 1e-8)
}

test_that("1D analytical masses own each boundary exactly once", {
  d <- hist_fixture(data.frame(x = c(0, 1, 2, 0, 6, 3)),
                    data.frame(x = c(6, 5, 2, 0, 6, 3)))
  h <- expect_no_warning(ggInterval_hist(d, aes(x = x), bins = 3))
  tab <- h[["Table x"]]
  expect_s3_class(h, "ggInterval_hist_result")
  expect_true("plot" %in% names(h))
  expect_identical(names(tab), c("Interval", "Observed.Frequency", "Relative.Frequency"))
  expect_equal(tab$Observed.Frequency,
               c(1/3 + 1/4 + 1, 1/3 + 1/2 + 2, 1/3 + 1/4 + 1),
               tolerance = 1e-8)
  expect_equal(sum(tab$Observed.Frequency), 6, tolerance = 1e-8)
  expect_equal(sum(tab$Relative.Frequency), 1, tolerance = 1e-8)
  expect_identical(as.character(tab$Interval), c("[0:2]", "[2:4]", "[4:6]"))
  layer <- expect_no_warning(ggplot_build(h$plot))$data[[1]]
  expect_equal(layer$y, tab$Observed.Frequency, tolerance = 1e-8)
  expect_equal(nrow(layer), 3)
  one <- expect_no_warning(ggInterval_hist(d, aes(x = x), bins = 1))[["Table x"]]
  expect_equal(one$Observed.Frequency, 6, tolerance = 1e-8)
  perm <- expect_no_warning(ggInterval_hist(d[c(6, 2, 4, 1, 5, 3), , drop = FALSE],
                                           aes(x = x), bins = 3))
  expect_equal(hist_tables(perm), hist_tables(h), tolerance = 1e-8)

  positive <- hist_fixture(data.frame(x = c(0, 0.5)),
                           data.frame(x = c(2, 1.5)))
  expect_equal(expect_no_warning(ggInterval_hist(positive, aes(x = x), bins = 2))[["Table x"]]$
                 Observed.Frequency, c(1, 1), tolerance = 1e-8)
})

test_that("constant 1D support is padded in both methods", {
  for (value in c(0, 1)) {
    d <- hist_fixture(data.frame(x = rep(value, 3)),
                      data.frame(x = rep(value, 3)))
    for (bins in c(3, 4)) {
      h <- expect_no_warning(ggInterval_hist(d, aes(x = x), bins = bins))
      f <- h[["Table x"]]$Observed.Frequency
      expect_true(all(is.finite(f)))
      expect_equal(sum(f), 3, tolerance = 1e-8)
      expect_equal(sum(f == 3), 1)
      expect_true(all(is.finite(h$plot$data$start)))
      expect_true(all(h$plot$data$end > h$plot$data$start))
    }
    h <- ggInterval_hist(d, aes(x = x), method = "unequal-bin")
    expect_identical(names(h[["Table x"]]), c("Interval", "Cumulative.Concepts"))
    expect_true(all(is.finite(h$plot$data$x1)))
    expect_equal(max(h[["Table x"]]$Cumulative.Concepts), 3,
                 tolerance = 1e-8)
  }
})

test_that("plotAll and grouping retain pooled breaks and per-group mass", {
  d <- hist_fixture(data.frame(x = c(0, 1, 2, 0, 6, 3), z = rep(1, 6)),
                    data.frame(x = c(6, 5, 2, 0, 6, 3), z = rep(1, 6)))
  selected <- expect_no_warning(ggInterval_hist(d, aes(x = x), bins = 3))
  all_vars <- ggInterval_hist(d, plotAll = TRUE, bins = 3)
  expect_equal(all_vars[["Table x"]], selected[["Table x"]], tolerance = 1e-8)
  expect_equal(sum(all_vars[["Table z"]]$Observed.Frequency), 6, tolerance = 1e-8)
  expect_true(all(is.finite(all_vars[["Table z"]]$Observed.Frequency)))
  all_unequal <- ggInterval_hist(d, plotAll = TRUE, method = "unequal-bin")
  expect_true(all(c("Table x", "Table z") %in% names(all_unequal)))

  assign("hist_group_g", c("a", "a", "a", "b", "b", "b"), envir = globalenv())
  perm_order <- c(5, 2, 6, 1, 4, 3)
  assign("hist_group_g_perm", hist_group_g[perm_order], envir = globalenv())
  on.exit(rm(list = c("hist_group_g", "hist_group_g_perm"), envir = globalenv()),
          add = TRUE)
  for (position in c("identity", "stack")) {
    grouped <- ggInterval_hist(d, aes(x = x, fill = hist_group_g), bins = 3,
                               position = position)
    tabs <- hist_tables(grouped)
    expect_length(tabs, 2)
    expect_identical(as.character(tabs[[1]]$Interval), as.character(tabs[[2]]$Interval))
    expect_identical(as.character(tabs[[1]]$Interval),
                     as.character(selected[["Table x"]]$Interval))
    for (tab in tabs) {
      expect_equal(sum(tab$Observed.Frequency), 3, tolerance = 1e-8)
      expect_equal(sum(tab$Relative.Frequency), 1, tolerance = 1e-8)
    }
    grouped_all <- hist_tables(ggInterval_hist(d, aes(fill = hist_group_g),
                              plotAll = TRUE, bins = 3, position = position))
    expect_equal(grouped_all[names(tabs)], tabs, tolerance = 1e-8)
    perm <- hist_tables(ggInterval_hist(d[perm_order, , drop = FALSE],
                       aes(x = x, fill = hist_group_g_perm), bins = 3,
                       position = position))
    expect_setequal(names(perm), names(tabs))
    expect_equal(perm[names(tabs)], tabs, tolerance = 1e-8)
  }
})

test_that("invalid bin counts fail only when used", {
  d <- hist_fixture(data.frame(x = c(0, 1), y = c(0, 0.5)),
                    data.frame(x = c(2, 1), y = c(2, 1.5)))
  invalid <- list(0, -1, 2.5, NA, NaN, Inf, c(2, 3), "3")
  for (bad in invalid) {
    expect_error(ggInterval_hist(d, aes(x = x), bins = bad), "bins")
    expect_no_error(ggInterval_hist(d, aes(x = x), bins = bad,
                                    method = "unequal-bin"))
    for (fun in list(ggInterval_2Dhist, ggInterval_2DhistMatrix)) {
      expect_error(fun(d, aes(x = x, y = y), xBins = bad, yBins = 2), "xBins")
      expect_error(fun(d, aes(x = x, y = y), xBins = 2, yBins = bad), "yBins")
      expect_no_error(fun(d, aes(x = x, y = y), xBins = bad, yBins = bad,
                          method = "unequal-bin"))
    }
  }
  expect_warning(ggInterval_hist(d, aes(x = x), bins = 201), "Bins")
  expect_error(ggInterval_hist(d, aes(x = x), bins = 1001), "Bins")
  expect_error(ggInterval_2Dhist(d, aes(x = x, y = y), xBins = 101,
                                yBins = 100), "Bins")
  expect_error(ggInterval_2DhistMatrix(d, xBins = 51, yBins = 50), "Bins")
  expect_hist_mass(ggInterval_2Dhist(d, aes(x = x, y = y),
                                    xBins = 1, yBins = 1)$plot$data, 2)
  m <- ggInterval_2DhistMatrix(d, xBins = 1, yBins = 1)$data
  expect_equal(m$f[m$isPlot], c(2, 2), tolerance = 1e-8)
})

test_that("2D point ownership and mixed widths conserve mass", {
  # x: [0,2] and the point 1 (internal break); y: [0,4] and the point 2 (internal break)
  d <- hist_fixture(data.frame(x = c(0, 1), y = c(0, 2)),
                    data.frame(x = c(2, 1), y = c(4, 2)))
  for (method in c("equal-bin", "unequal-bin")) {
    h <- ggInterval_2Dhist(d, aes(x = x, y = y), method = method,
                           xBins = 2, yBins = 2, display = "f")
    expect_type(h, "list")
    expect_true(all(c("plot", "Table (x, y)") %in% names(h)))
    cells <- h$plot$data
    expect_equal(cells$f, c(0.25, 0.25, 0.25, 1.25), tolerance = 1e-8)
    expect_hist_mass(cells, 2)

    mixed <- hist_fixture(data.frame(x = c(0, 1, 0), y = c(0, 0, 1)),
                          data.frame(x = c(2, 1, 2), y = c(2, 2, 1)))
    cells <- ggInterval_2Dhist(mixed, aes(x = x, y = y), method = method,
                               xBins = 2, yBins = 2)$plot$data
    expect_equal(cells$f, c(0.25, 0.75, 0.75, 1.25), tolerance = 1e-8)
    expect_hist_mass(cells, 3)
    for (both in c(FALSE, TRUE)) {
      constant <- hist_fixture(
        data.frame(x = c(1, 1), y = if (both) c(0, 0) else c(0, 1)),
        data.frame(x = c(1, 1), y = if (both) c(0, 0) else c(2, 1)))
      cells <- ggInterval_2Dhist(constant, aes(x = x, y = y), method = method,
                                 xBins = 3, yBins = 4)$plot$data
      expect_hist_mass(cells, 2)
      if (both) expect_equal(sum(cells$f > 0), 1)
    }
    merged <- ggInterval_2Dhist(mixed, aes(x = x, y = y),
                               method = "unequal-bin", tau = 1.1)$plot$data
    expect_hist_mass(merged, 3)
  }
})

test_that("matrix panels including a constant variable conserve mass", {
  d <- hist_fixture(data.frame(x = c(0, 1, 0), y = c(0, 0, 1), z = c(1, 1, 1)),
                    data.frame(x = c(2, 1, 2), y = c(2, 2, 1), z = c(1, 1, 1)))
  for (method in c("equal-bin", "unequal-bin")) {
    cells <- ggInterval_2DhistMatrix(d, method = method,
                                     xBins = 2, yBins = 3)$data
    panels <- split(cells[cells$isPlot, ],
                    interaction(cells$xv[cells$isPlot], cells$yv[cells$isPlot],
                                drop = TRUE))
    expect_length(panels, 6)
    for (panel in panels) expect_hist_mass(panel, 3)
  }
})

test_that("shared helpers preserve exact breaks and reject invalid endpoints", {
  allocate <- ggInterval:::interval_overlap_ratio
  breaks <- ggInterval:::build_hist_equal_breaks
  expect_equal(allocate(1, 5, c(0, 2, 4, 6)), c(0.25, 0.5, 0.25),
               tolerance = 1e-8)
  for (point in c(0, 1, 2)) {
    expect_equal(allocate(point, point, c(0, 1, 2)),
                 if (point == 0) c(1, 0) else c(0, 1), tolerance = 1e-8)
  }
  expect_equal(allocate(-1, -1, c(0, 1, 2)), c(0, 0), tolerance = 1e-8)
  expect_equal(allocate(3, 3, c(0, 1, 2)), c(0, 0), tolerance = 1e-8)
  expect_identical(breaks(0.1, 0.9, 7), seq(0.1, 0.9, length.out = 8))
  expect_error(breaks(0, 1, c(2, 3)), "bins")
  for (bad in c(NA_real_, NaN, Inf, -Inf)) {
    expect_error(breaks(c(0, bad), c(1, 2), 3), "finite")
    expect_error(allocate(bad, 1, c(0, 1, 2)), "finite")
    expect_error(ggInterval:::build_hist2d_breaks(
      list(min = c(0, bad), max = c(1, 2)), NULL, "unequal-bin"), "finite")
  }
  expect_error(allocate(2, 1, c(0, 1, 2)), "upper endpoints")
})

test_that("facedata regressions and related return contracts are retained", {
  h <- expect_no_warning(ggInterval_hist(facedata, aes(x = AD), bins = 10))
  tab <- h[["Table AD"]]
  expect_equal(nrow(tab), 10)
  expect_equal(sum(tab$Observed.Frequency), 27, tolerance = 1e-8)
  expect_equal(sum(tab$Relative.Frequency), 1, tolerance = 1e-8)
  layer <- expect_no_warning(ggplot_build(h$plot))$data[[1]]
  expect_equal(layer$y, tab$Observed.Frequency, tolerance = 1e-8)
  expect_equal(nrow(layer), 10)
  h2 <- ggInterval_2Dhist(facedata, aes(x = BC, y = AD),
                         xBins = 8, yBins = 8, display = "f")
  expect_hist_mass(h2$plot$data, 27)
  tab2 <- as.matrix(hist_tables(h2)[[1]])
  expect_equal(dim(tab2), c(9L, 9L))
  expect_equal(tab2[1:8, 9], rowSums(tab2[1:8, 1:8]),
               ignore_attr = TRUE, tolerance = 1e-8)
  expect_equal(tab2[9, 1:8], colSums(tab2[1:8, 1:8]),
               ignore_attr = TRUE, tolerance = 1e-8)
  expect_equal(unname(tab2[9, 9]), sum(tab2[1:8, 1:8]), tolerance = 1e-8)
  pca <- ggInterval_PCA(facedata, plot = FALSE)
  expect_s3_class(pca, "princomp")
  expect_true(all(c("ggplotPCA", "scores_interval") %in% names(pca)))
  expect_s3_class(scale(facedata), "ggInterval")
  expect_true(RSDA::is.sym.interval(scale(facedata$AD)))
})


test_that("unequal-bin constants have exact coordinates and bounded output", {
  expected_labels <- c("[0:0]", "[1:1]", "[10.3:10.3]", "[-11.1:-11.1]",
                       "[0.003:0.003]", "[1e+08:1e+08]")
  values <- c(0, 1, 10.3, -11.1, 0.003, 1e8)
  for (n in c(1, 2, 3)) {
    for (i in seq_along(values)) {
      v <- values[i]
      d <- hist_fixture(data.frame(x = rep(v, n)),
                        data.frame(x = rep(v, n)))
      h <- ggInterval_hist(d, aes(x = x), method = "unequal-bin")
      cells <- h$plot$data
      tab <- h[["Table x"]]
      expect_identical(names(tab), c("Interval", "Cumulative.Concepts"))
      expect_true(all(is.finite(as.matrix(cells[, c("x1", "x2", "y1", "y2")]))))
      expect_equal(nrow(cells), 1)
      expect_equal(nrow(tab), 1)
      nonzero <- cells[cells$y2 > 0, , drop = FALSE]
      expect_equal(nrow(nonzero), 1)
      expect_identical(nonzero$x1, v)
      expect_identical(nonzero$x2, v)
      expect_equal(nonzero$y1, 0)
      expect_equal(nonzero$y2, n)
      expect_equal(sum(tab$Cumulative.Concepts > 0), 1)
      expect_true(all(is.finite(tab$Cumulative.Concepts)))
      expect_equal(tab$Cumulative.Concepts, n)
      expect_identical(as.character(tab$Interval), expected_labels[i])
    }
  }
})

test_that("unequal-bin plotAll keeps constants distinct and nonconstants unchanged", {
  n <- 3
  lower <- data.frame(a = rep(2.5, n), b = rep(-7, n), x = c(0, 0, 1.5))
  upper <- data.frame(a = rep(2.5, n), b = rep(-7, n), x = c(2, 1, 3))
  d <- hist_fixture(lower, upper)
  h <- ggInterval_hist(d, plotAll = TRUE, method = "unequal-bin")
  expect_true(all(is.finite(as.matrix(h$plot$data[, c("x1", "x2", "y1", "y2")]))))
  for (var in c("a", "b")) {
    v <- lower[[var]][1]
    cells <- h$plot$data[h$plot$data$group == var, , drop = FALSE]
    tab <- h[[paste0("Table ", var)]]
    expect_identical(names(tab), c("Interval", "Cumulative.Concepts"))
    expect_equal(nrow(cells), 1)
    expect_equal(nrow(tab), 1)
    expect_equal(sum(cells$y2 > 0), 1)
    expect_identical(cells$x1, v)
    expect_identical(cells$x2, v)
    expect_equal(cells$y1, 0)
    expect_equal(cells$y2, n)
    expect_true(all(is.finite(tab$Cumulative.Concepts)))
    expect_equal(tab$Cumulative.Concepts, n)
    expect_identical(as.character(tab$Interval),
                     paste0("[", paste(v, v, sep = ":"), "]"))
  }
  selected_data <- hist_fixture(lower["x"], upper["x"])
  selected <- ggInterval_hist(selected_data, aes(x = x), method = "unequal-bin")
  expect_equal(h[["Table x"]], selected[["Table x"]], tolerance = 1e-12)
  all_cells <- h$plot$data[h$plot$data$group == "x", c("x1", "x2", "y1", "y2")]
  selected_cells <- selected$plot$data[, c("x1", "x2", "y1", "y2")]
  rownames(all_cells) <- NULL
  rownames(selected_cells) <- NULL
  expect_equal(all_cells, selected_cells, tolerance = 1e-12)
})

test_that("unequal-bin first runs retain shared-minimum coverage", {
  lower <- data.frame(x = c(0, 0, 1.5))
  upper <- data.frame(x = c(2, 1, 3))
  d <- hist_fixture(lower, upper)
  grid <- seq(0, 3, 0.01)
  coverage <- vapply(round(grid, 2), function(point) {
    sum(lower$x <= point & point <= upper$x)
  }, numeric(1))
  runs <- rle(coverage)
  ends <- cumsum(runs$lengths)
  starts <- c(1L, head(ends, -1L) + 1L)
  expected_start <- grid[starts]
  expected_end <- grid[ends]
  h <- ggInterval_hist(d, aes(x = x), method = "unequal-bin")
  tab <- h[["Table x"]]
  expect_equal(h$plot$data$x1, expected_start, tolerance = 1e-12)
  expect_equal(h$plot$data$x2, expected_end, tolerance = 1e-12)
  expect_equal(tab$Cumulative.Concepts, runs$values)
  expect_identical(as.character(tab$Interval),
                   paste0("[", paste(expected_start, expected_end, sep = ":"), "]"))

  face <- ggInterval_hist(facedata, aes(x = AD), method = "unequal-bin")
  expect_identical(as.character(face[["Table AD"]]$Interval[1]), "[149.34:150.32]")
  expect_equal(face[["Table AD"]]$Cumulative.Concepts[1], 2)
  expect_equal(face$plot$data$x1[1], 149.34, tolerance = 1e-12)
  expect_equal(face$plot$data$x2[1], 150.32, tolerance = 1e-12)
  covering <- facedata$AD$min <= 149.34 & facedata$AD$max >= 149.34
  expect_equal(sum(covering), 2)
  expect_setequal(rownames(facedata)[covering], c("KHA1", "KHA2"))
})
