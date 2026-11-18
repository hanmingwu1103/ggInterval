library(ggInterval)

test_that("scatterplot mappings preserve endpoint geometry and alpha precedence", {
  pdf_path <- tempfile(fileext = ".pdf")
  grDevices::pdf(pdf_path)
  on.exit({ grDevices::dev.off(); unlink(pdf_path) }, add = TRUE)
  input <- facedata[1:10, ]
  default <- expect_no_warning(ggInterval_scatterplot(input, aes(AD, BC)))
  mapped <- expect_no_warning(ggInterval_scatterplot(input, aes(AD, BC, alpha = 0.2)))
  default_built <- expect_no_warning(ggplot_build(default))
  mapped_built <- expect_no_warning(ggplot_build(mapped))

  # The built-in alpha mapping has historically taken precedence. A constant
  # alpha supplied outside aes() remains the supported explicit override.
  expect_equal(mapped_built$data, default_built$data)
  expect_equal(default_built$data[[1]]$alpha, rep(0.55, nrow(input)))
  expect_equal(ggplot_build(mapped + scale_alpha_identity())$data[[1]]$alpha,
               rep(0.5, nrow(input)))
  fixed <- expect_no_warning(ggInterval_scatterplot(input, aes(AD, BC), alpha = 0.2))
  expect_equal(expect_no_warning(ggplot_build(fixed))$data[[1]]$alpha,
               rep(0.2, nrow(input)))
  expect_equal(mapped_built$data[[1]]$xmin, input$AD$min)
  expect_equal(mapped_built$data[[1]]$xmax, input$AD$max)
  expect_equal(mapped_built$data[[1]]$ymin, input$BC$min)
  expect_equal(mapped_built$data[[1]]$ymax, input$BC$max)
  expect_identical(mapped_built$data[[2]]$label, rownames(input))

  example_plot <- mapped + scale_fill_manual(
    labels = rownames(input), values = rainbow(nrow(input)), name = "Group"
  )
  expect_no_warning(ggplotGrob(example_plot))
  expect_no_warning(print(example_plot))
})

test_that("3D scatterplot axes retain projected coordinates and render without warnings", {
  pdf_path <- tempfile(fileext = ".pdf")
  grDevices::pdf(pdf_path)
  on.exit({ grDevices::dev.off(); unlink(pdf_path) }, add = TRUE)
  input <- facedata[1:5, ]
  p <- expect_no_warning(ggInterval_3Dscatterplot(input, aes(x = BC, y = EH, z = GH)))
  built <- expect_no_warning(ggplot_build(p))
  projected_lower <- round(abs(input$EH$min) / sqrt(2), 6)
  projected_upper <- round(abs(input$EH$max) / sqrt(2), 6)
  expect_equal(range(built$data[[1]]$x),
               c(min(input$BC$min + projected_lower),
                 max(input$BC$max + projected_upper)))
  expect_equal(range(built$data[[1]]$y),
               c(min(input$GH$min + projected_lower),
                 max(input$GH$max + projected_upper)))
  expect_equal(built$data[[2]]$x, built$data[[1]]$x)
  expect_equal(built$data[[2]]$y, built$data[[1]]$y)

  # Retain the original axis draw count as well as coordinates: reducing the
  # count changes antialiased edges when previously approved PDFs are rendered.
  axes <- lapply(built$data[3:5], function(d) d[, c("x", "y", "xend", "yend")])
  expect_equal(vapply(axes, nrow, integer(1)), rep(120L, 3))
  unique_axes <- lapply(axes, unique)
  expect_equal(vapply(unique_axes, nrow, integer(1)), rep(1L, 3))
  expect_equal(unique_axes[[1]]$x, unique_axes[[2]]$x)
  expect_equal(unique_axes[[1]]$x, unique_axes[[3]]$x)
  expect_equal(unique_axes[[1]]$y, unique_axes[[2]]$y)
  expect_equal(unique_axes[[1]]$y, unique_axes[[3]]$y)
  expect_equal(unique_axes[[2]]$yend, unique_axes[[2]]$y)
  expect_equal(unique_axes[[3]]$xend, unique_axes[[3]]$x)
  upper <- max(c(built$data[[1]]$x, built$data[[1]]$y))
  expect_equal(unique_axes[[1]]$xend, upper)
  expect_equal(unique_axes[[1]]$yend, upper)
  expect_equal(unique_axes[[2]]$xend, upper)
  expect_equal(unique_axes[[3]]$yend, upper)
  expect_identical(built$data[[6]]$label, "EH")
  expect_no_warning(ggplotGrob(p))
  expect_no_warning(print(p))
})
