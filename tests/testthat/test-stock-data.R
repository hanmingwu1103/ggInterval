stock_data_load <- function() {
  e <- new.env(parent = emptyenv())
  utils::data("stock.data.LH", package = "ggInterval", envir = e)
  e$stock.data.LH
}

stock_data_reconstruct <- function() {
  e <- new.env(parent = emptyenv())
  utils::data("ttrc", package = "TTR", envir = e)
  d <- subset(e$ttrc[, c("Date", "Close", "Low", "High")],
              format(Date, "%Y-%m") %in% c("1985-01", "1985-02", "1985-03"))
  d$Month <- factor(month.abb[as.integer(format(d$Date, "%m"))],
                    levels = month.abb[1:3])
  d$Day <- as.integer(format(d$Date, "%d"))
  classic2sym(d, groupby = "customize", minData = d$Low, maxData = d$High)
}

test_that("the stored financial data are a directly inspectable symbolic table", {
  d <- stock_data_load()
  expect_s3_class(d, "symbolic_tbl")
  expect_identical(dim(d), c(62L, 7L))
  expect_identical(names(d), c("V1", "Date", "Close", "Low", "High", "Month", "Day"))
  expect_s3_class(d$V1, "symbolic_interval")
  expect_s3_class(d$Date, "Date")
  expect_identical(levels(d$Month), month.abb[1:3])
  expect_type(d$Day, "integer")
  expect_identical(as.integer(table(d$Month)), c(22L, 19L, 21L))
  expect_identical(range(d$Date), as.Date(c("1985-01-02", "1985-03-29")))
  expect_identical(d$V1$min, d$Low)
  expect_identical(d$V1$max, d$High)
  expect_equal(nrow(head(d)), 6L)
  expect_identical(unserialize(serialize(d, NULL)), d)
})

test_that("the financial data exactly reconstruct from the documented TTR subset", {
  skip_if_not_installed("TTR")
  old <- stock_data_reconstruct()
  reconstructed <- dplyr::bind_cols(old$intervalData, old$rawData)
  expect_identical(stock_data_load(), reconstructed)
})

test_that("the stored table preserves the original financial plot layer data", {
  skip_if_not_installed("TTR")
  old <- stock_data_reconstruct()
  draw <- function(d) {
    ggInterval_lineplot(d, aes(y = V1, x = Day), barWidth = 0.6) +
      geom_point(aes(y = Close), shape = 21, fill = "#D95F02",
                 color = "black", size = 1.6, stroke = 0.2) +
      coord_cartesian(xlim = c(1, 31), expand = FALSE) +
      facet_wrap(~Month, ncol = 1, scales = "free_y") +
      scale_x_continuous(breaks = c(1, 8, 15, 22, 29)) +
      labs(title = "Interval-valued Line Plot", x = "Day of month", y = "Price") +
      ggthemes::theme_economist() +
      theme(strip.text = element_text(face = "bold"))
  }
  old_plot <- ggplot2::ggplot_build(draw(old))
  new_plot <- ggplot2::ggplot_build(draw(stock_data_load()))
  expect_identical(new_plot$data, old_plot$data)
  expect_identical(new_plot$layout$layout, old_plot$layout$layout)
  expect_identical(lapply(new_plot$layout$panel_params, function(x) x$y.range),
                   lapply(old_plot$layout$panel_params, function(x) x$y.range))
})
