# The INCOMELEVEL labels of Environment follow the SODAS source order since 0.2.6.
test_that("Environment modal labels follow the SODAS source order", {
  expected <- list(
    URBANICITY = c("6", "4", "5", "1", "3", "2"),
    INCOMELEVEL = c("25", "75", "50", "90", "100"),
    EDUCATION = c("1", "3", "5", "6"),
    REGIONDEVELOPME = c("4", "3", "2", "1")
  )
  expect_identical(dim(Environment), c(14L, 17L))
  for (name in names(expected)) {
    column <- unclass(Environment[[name]])
    expect_length(column, 14L)
    for (object in column) {
      expect_identical(object$var, expected[[name]])
      expect_length(object$prop, length(expected[[name]]))
      expect_equal(sum(object$prop), 1, tolerance = 1e-5)
    }
  }
})

test_that("Environment INCOMELEVEL proportions keep their stored positions", {
  income <- unclass(Environment$INCOMELEVEL)
  expect_equal(income[[1]]$prop, c(0.911765, 0.0196078, 0.0686275, 0, 0), tolerance = 1e-6)
  expect_equal(round(income[[4]]$prop, 3), c(0.117, 0.181, 0.074, 0.277, 0.351))
  expect_equal(round(income[[6]]$prop, 3), c(0.038, 0.125, 0.062, 0.225, 0.550))
  modal_summary <- summary(Environment)$symbolic_modal
  expect_identical(as.character(trimws(modal_summary[1:5, "INCOMELEVEL"])),
                   c("25: 0.32", "75: 0.20", "50: 0.25", "90: 0.13", "100: 0.10"))
})
