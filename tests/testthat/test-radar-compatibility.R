radar_with_pdf <- function(code) {
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path, width = 8, height = 7)
  on.exit({
    grDevices::dev.off()
    unlink(path)
  }, add = TRUE)
  force(code)
}

radar_render_pdf <- function(plot) radar_with_pdf(print(plot))

test_that("mixed radar displays build and render without compatibility warnings", radar_with_pdf({
  for (type in c("default", "rect")) {
    plot <- expect_no_warning(ggInterval_radarplot(
      Environment, plotPartial = c(4, 6), showLegend = FALSE,
      base_circle = FALSE, base_lty = 1, addText = FALSE,
      addText_modal = FALSE, type = type
    ))
    built <- expect_no_warning(ggplot2::ggplot_build(plot))
    expect_no_warning(radar_render_pdf(plot))

    alpha_scale <- built$plot$scales$get_scales("alpha")
    keys <- alpha_scale$get_limits()
    expect_length(keys, 26L)
    expect_equal(unname(alpha_scale$map(keys)), seq(0.1, 1, length.out = 26L))
    origin <- which(vapply(plot$layers, function(layer) {
      inherits(layer$geom, "GeomPoint") && "alpha" %in% names(layer$mapping)
    }, logical(1)))
    expect_length(origin, 1L)
    expect_true(all(built$data[[origin]]$x == 0))
    expect_true(all(built$data[[origin]]$y == 0))
    expect_equal(built$data[[origin]]$alpha,
                 unname(alpha_scale$map(plot$layers[[origin]]$data$Variables)))
  }
}))

test_that("each hidden radar guide retains its own observation", radar_with_pdf({
  plot <- expect_no_warning(ggInterval_radarplot(
    Environment, plotPartial = c(4, 6), showLegend = FALSE,
    base_circle = FALSE, addText = FALSE, addText_modal = FALSE
  ))
  built <- expect_no_warning(ggplot2::ggplot_build(plot))
  guides <- which(vapply(plot$layers, function(layer) {
    inherits(layer$geom, "GeomPath") && identical(layer$aes_params$linetype, 0)
  }, logical(1)))
  expect_length(guides, 4L)
  lower <- built$data[guides[c(1, 3)]]
  upper <- built$data[guides[c(2, 4)]]
  expect_false(identical(lower[[1]]$x, lower[[2]]$x))
  expect_false(identical(upper[[1]]$y, upper[[2]]$y))
  for (j in seq_along(guides)) {
    layer <- plot$layers[[guides[j]]]
    bound <- if (j %% 2L == 1L) "1" else "2"
    expect_equal(built$data[[guides[j]]]$x, layer$data[[paste0("x", bound)]])
    expect_equal(built$data[[guides[j]]]$y, layer$data[[paste0("y", bound)]])
    expect_true(all(built$data[[guides[j]]]$linetype == 0))
  }
}))

test_that("radar legends, text, circles and quantile bands build and render", radar_with_pdf({
  numeric_data <- Environment[, vapply(Environment, RSDA::is.sym.interval,
                                        logical(1))]
  cases <- list(
    list(data = numeric_data, plotPartial = c(4, 6), type = "default"),
    list(data = numeric_data, plotPartial = c(4, 6), type = "rect"),
    list(data = Environment, plotPartial = c(4, 6), type = "default",
         addText_modal.p = TRUE),
    list(data = facedata, type = "quantile", quantileNum = 5, Drift = 0)
  )
  for (args in cases) {
    plot <- expect_no_warning(do.call(ggInterval_radarplot, args))
    built <- expect_no_warning(ggplot2::ggplot_build(plot))
    expect_true(length(built$data) > 0L)
    expect_no_warning(radar_render_pdf(plot))
  }
}))

test_that("separate radar panels build and render without compatibility warnings", radar_with_pdf({
  numeric_data <- Environment[, vapply(Environment, RSDA::is.sym.interval,
                                        logical(1))]
  for (input in list(Environment, numeric_data)) {
    for (type in c("default", "rect")) {
      # marrangeGrob builds the component ggplots during this call.
      panels <- expect_no_warning(ggInterval_radarplot(
        input, plotPartial = c(4, 6), inOneFig = FALSE, type = type,
        showLegend = TRUE, base_circle = TRUE, addText = TRUE
      ))
      expect_s3_class(panels, "arrangelist")
      expect_no_warning(radar_render_pdf(panels))
    }
  }
}))

test_that("pure modal radar displays need no interval alpha scale", radar_with_pdf({
  modal_data <- Environment[, !vapply(Environment, RSDA::is.sym.interval,
                                      logical(1))]
  for (type in c("default", "rect")) {
    plot <- expect_no_warning(ggInterval_radarplot(
      modal_data, plotPartial = c(4, 6), type = type,
      base_circle = FALSE, addText = FALSE, addText_modal = FALSE
    ))
    built <- expect_no_warning(ggplot2::ggplot_build(plot))
    expect_null(built$plot$scales$get_scales("alpha"))
    expect_no_warning(radar_render_pdf(plot))
  }
}))
