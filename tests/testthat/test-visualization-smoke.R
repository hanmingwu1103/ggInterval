library(ggInterval)

is_ggplot_object <- function(x) {
  inherits(x, "ggplot") || inherits(x, "ggplot2::ggplot")
}

test_that("classic2sym returns a ggInterval object", {
  iris_sym <- classic2sym(iris, groupby = "Species")
  expect_s3_class(iris_sym, "ggInterval")
  expect_true("intervalData" %in% names(iris_sym))
})

test_that("scatterplot label controls return a ggplot object", {
  Subjects <- substr(rownames(facedata), 1, 3)
  p <- ggInterval_scatterplot(
    facedata,
    aes(x = AD, y = BC, fill = Subjects),
    showLabels = TRUE,
    labelSize = 2.5,
    labelPosition = "topright",
    labelNudgeX = 0.2,
    labelNudgeY = 0.1,
    checkOverlap = TRUE
  )

  expect_true(is_ggplot_object(p))

  built <- ggplot_build(p)
  expect_equal(built$data[[2]]$x[1], facedata$AD$max[1] + 0.2)
  expect_equal(built$data[[2]]$y[1], facedata$BC$max[1] + 0.1)
})

test_that("scatterplot matrix returns a ggplot object and honors borderLinewidth", {
  p <- ggInterval_scatterMatrix(
    facedata[, 1:3],
    borderLinewidth = 0.15
  )

  expect_true(is_ggplot_object(p))
  built <- ggplot_build(p)
  expect_true(any(abs(built$data[[1]]$linewidth - 0.15) < 1e-8))
})

test_that("histogram variants return plots", {
  h_equal <- ggInterval_hist(facedata, aes(x = AD), method = "equal-bin", bins = 8)
  h_unequal <- ggInterval_hist(facedata, aes(x = AD), method = "unequal-bin")

  expect_s3_class(h_equal, "ggInterval_hist_result")
  expect_s3_class(h_unequal, "ggInterval_hist_result")
  expect_true(is_ggplot_object(h_equal$plot))
  expect_true(is_ggplot_object(h_unequal$plot))
})

test_that("2D histogram variants return plots and tables", {
  h_equal <- ggInterval_2Dhist(
    facedata,
    aes(x = BC, y = AD),
    xBins = 8,
    yBins = 8,
    display = "p",
    palette = "Blues",
    cell_labels = TRUE
  )
  h_unequal <- ggInterval_2Dhist(
    facedata,
    aes(x = BC, y = AD),
    method = "unequal-bin",
    display = "h",
    tau = 0.5
  )

  expect_true(is_ggplot_object(h_equal$plot))
  expect_true(is_ggplot_object(h_unequal$plot))
  expect_true(any(grepl("^Table", names(h_equal))))
  expect_true(any(grepl("^Table", names(h_unequal))))
  expect_true(any(vapply(
    h_equal$plot$layers,
    function(layer) inherits(layer$geom, "GeomText"),
    logical(1)
  )))
})

test_that("2D histogram matrices support equal-bin and unequal-bin methods", {
  m_equal <- ggInterval_2DhistMatrix(
    facedata,
    method = "equal-bin",
    xBins = 6,
    yBins = 6,
    display = "p",
    palette = "Blues",
    cell_labels = TRUE
  )
  m_unequal <- ggInterval_2DhistMatrix(
    facedata,
    method = "unequal-bin",
    display = "h",
    palette = "Blues",
    tau = 0.5,
    cell_labels = FALSE
  )

  expect_true(is_ggplot_object(m_equal))
  expect_true(is_ggplot_object(m_unequal))
})

test_that("PCA label controls return a ggplot object", {
  pca_fit <- ggInterval_PCA(
    facedata,
    plot = FALSE,
    concepts_group = substr(rownames(facedata), 1, 3),
    showLabels = TRUE,
    labelSize = 2.5,
    checkOverlap = TRUE
  )

  expect_true(is_ggplot_object(pca_fit$ggplotPCA))
})

test_that("correlation heatmap returns a ggplot object", {
  p <- ggInterval_corrplot(facedata, method = "BG", triangle = "lower")
  expect_true(is_ggplot_object(p))
})

test_that("boxplot width types return ggplot objects", {
  p_violin <- ggInterval_boxplot(facedata, plotAll = TRUE, width_type = "violin-like")
  p_depth <- ggInterval_boxplot(facedata, aes(x = AD), width_type = "quantile-depth")
  p_side <- ggInterval_boxplot(facedata, plotAll = TRUE, width_type = "side-by-side")

  expect_true(is_ggplot_object(p_violin))
  expect_true(is_ggplot_object(p_depth))
  expect_true(is_ggplot_object(p_side))
})

test_that("index plots support built-in row ordering for plotAll", {
  p_original <- ggInterval_indexplot(facedata, plotAll = TRUE)
  p_centers <- ggInterval_indexplot(
    facedata,
    plotAll = TRUE,
    row_order = "c"
  )
  p_ranges <- ggInterval_indexplot(
    facedata,
    plotAll = TRUE,
    row_order = "r"
  )
  custom_order <- c(19:21, 22:24, 1:18, 25:27)
  p_user <- ggInterval_indexplot(
    facedata,
    plotAll = TRUE,
    row_order = "u",
    user_order = custom_order
  )

  split_labels <- function(plot_obj) {
    split(plot_obj$data$row_label, plot_obj$data$g)
  }
  centers_by_panel <- split_labels(p_centers)
  ranges_by_panel <- split_labels(p_ranges)
  original_by_panel <- split_labels(p_original)
  user_by_panel <- split_labels(p_user)

  expect_identical(p_original$scales$get_scales("y")$labels, rownames(facedata))
  expect_true(all(vapply(
    original_by_panel,
    identical,
    logical(1),
    y = rownames(facedata)
  )))
  expect_true(all(vapply(
    names(centers_by_panel),
    function(var_name) identical(
      centers_by_panel[[var_name]],
      rownames(facedata)[order((facedata[[var_name]]$min + facedata[[var_name]]$max) / 2)]
    ),
    logical(1)
  )))
  expect_true(all(vapply(
    names(ranges_by_panel),
    function(var_name) identical(
      ranges_by_panel[[var_name]],
      rownames(facedata)[order(facedata[[var_name]]$max - facedata[[var_name]]$min)]
    ),
    logical(1)
  )))
  expect_true(all(vapply(
    user_by_panel,
    identical,
    logical(1),
    y = rownames(facedata)[custom_order]
  )))
  expect_true(all(p_centers$scales$get_scales("y")$labels == ""))
  expect_true(all(p_ranges$scales$get_scales("y")$labels == ""))
  expect_true(all(p_user$scales$get_scales("y")$labels == ""))
})

test_that("ordered index plots can repeat row labels within each panel", {
  p_labeled <- ggInterval_indexplot(
    facedata,
    plotAll = TRUE,
    row_order = "c",
    labels = TRUE
  )

  expect_true(any(vapply(
    p_labeled$layers,
    function(layer) inherits(layer$geom, "GeomText"),
    logical(1)
  )))
})

test_that("line plot aliases return ggplot objects with raw-data columns", {
  dates <- as.Date("2021-01-15") + c(0, 4, 5, 6, 7, 10)
  stock.data <- data.frame(
    Date = rep(dates, 2),
    Company = factor(rep(c("A", "B"), each = length(dates))),
    Close = c(100, 101.5, 102, 98.5, 99.5, 100.8,
              150, 151.2, 152.4, 153.5, 154.1, 155.3),
    Low = c(98.9, 100.2, 100.8, 96.9, 98.1, 99.4,
            148.7, 149.8, 151.0, 152.0, 152.7, 154.0),
    High = c(101.3, 102.6, 103.0, 100.3, 100.8, 102.0,
             151.5, 152.5, 153.6, 154.7, 155.2, 156.5)
  )
  stock.i <- classic2sym(
    stock.data,
    groupby = "customize",
    minData = stock.data$Low,
    maxData = stock.data$High
  )

  p_line <- ggInterval_lineplot(
    stock.i,
    aes(y = V1, x = Date, group = Company, fill = Company),
    add_line = FALSE
  ) +
    geom_line(aes(y = Close, color = Company, group = Company), linewidth = 0.4) +
    facet_wrap(~Company, ncol = 1, scales = "free_y")
  p_alias <- ggInterval_tsplot(
    stock.i,
    aes(y = V1, x = Date, group = Company, fill = Company)
  )

  expect_true(is_ggplot_object(p_line))
  expect_true(is_ggplot_object(p_alias))
  expect_true(all(c("Company", "Close", "Date") %in% names(p_line$data)))
})
