test_that("the installed aggregation CSV retains its input and cleanup contract", {
  path <- system.file("extdata", "data.csv", package = "ggInterval", mustWork = TRUE)
  input <- expect_no_warning(read.csv(path))
  expect_identical(dim(input), c(569L, 33L))
  expect_identical(tail(names(input), 1L), "X")
  expect_true(all(is.na(input$X)))
  expect_identical(names(input)[1:2], c("id", "diagnosis"))
  expect_identical(length(unique(input$id)), 569L)
  expect_equal(input$id[1], 842302)
  expect_identical(sort(unique(input$diagnosis)), c("B", "M"))
  expect_identical(as.integer(table(input$diagnosis)), c(357L, 212L))

  input$X <- NULL
  expect_identical(dim(input), c(569L, 32L))
  features <- input[, setdiff(names(input), c("id", "diagnosis")), drop = FALSE]
  expect_identical(ncol(features), 30L)
  expect_true(all(vapply(features, is.numeric, logical(1))))
  expect_true(all(is.finite(as.matrix(features))))
  expect_equal(input$radius_mean[1], 17.99)
})

test_that("installed aggregation input reproduces diagnosis-based endpoint summaries", {
  input <- read.csv(system.file("extdata", "data.csv",
                                package = "ggInterval", mustWork = TRUE))
  input$X <- NULL
  input <- input[, setdiff(names(input), "id"), drop = FALSE]
  feature_names <- setdiff(names(input), "diagnosis")
  groups <- split(input[, feature_names, drop = FALSE], input$diagnosis)
  lower <- do.call(rbind, lapply(groups, function(x) vapply(x, min, numeric(1))))
  upper <- do.call(rbind, lapply(groups, function(x) vapply(x, max, numeric(1))))

  result <- expect_no_warning(classic2sym(input, groupby = "diagnosis"))
  expect_identical(rownames(result$intervalData), names(groups))
  expect_identical(names(result$intervalData), feature_names)
  expect_identical(dim(result$intervalData), c(2L, 30L))
  for (j in seq_along(feature_names)) {
    expect_equal(result$intervalData[[j]]$min, unname(lower[, j]))
    expect_equal(result$intervalData[[j]]$max, unname(upper[, j]))
  }
  expect_equal(as.matrix(result$statisticsDF$min), unname(lower), ignore_attr = TRUE)
  expect_equal(as.matrix(result$statisticsDF$median), unname((lower + upper) / 2),
               ignore_attr = TRUE)
  expect_equal(as.matrix(result$statisticsDF$max), unname(upper), ignore_attr = TRUE)
})
