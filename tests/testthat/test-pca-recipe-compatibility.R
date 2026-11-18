test_that("RSDA PCA recipe preserves intervals and renders without row-name warnings", {
  verify_pca_recipe <- function() {
    drawing_file <- tempfile(fileext = ".pdf")
    grDevices::pdf(drawing_file)
    drawing_device <- grDevices::dev.cur()
    on.exit({
      if (drawing_device %in% grDevices::dev.list()) {
        grDevices::dev.off(drawing_device)
      }
      unlink(drawing_file)
    }, add = TRUE)
    pca.results <- RSDA::sym.pca(facedata, method = "tops")
    original_scores <- pca.results$Sym.Components
    pca.results$Sym.Components <- structure(
      as.data.frame(pca.results$Sym.Components),
      class = c("symbolic_tbl", "data.frame"))
  
    expect_identical(class(pca.results$Sym.Components),
                     c("symbolic_tbl", "data.frame"))
    expect_identical(names(pca.results$Sym.Components), names(original_scores))
    for (j in seq_along(original_scores)) {
      expect_identical(pca.results$Sym.Components[[j]], original_scores[[j]])
    }
    expect_no_warning(
      rownames(pca.results$Sym.Components) <- rownames(facedata)
    )
    expect_identical(rownames(pca.results$Sym.Components), rownames(facedata))
  
    Subjects <- substr(rownames(facedata), 1, 3)
    p <- ggInterval_scatterplot(
      pca.results$Sym.Components,
      aes(Dim.1, Dim.2, fill = as.factor(Subjects)),
      labelSize = 2.6, labelPosition = "topright",
      labelNudgeX = -0.2, labelNudgeY = 0.15,
      checkOverlap = TRUE, color = "black"
    ) +
      scale_fill_brewer(palette = "Set1") +
      scale_x_continuous(expand = expansion(mult = c(0.08, 0.12))) +
      scale_y_continuous(expand = expansion(mult = c(0.06, 0.10))) +
      coord_fixed(ratio = 1) +
      labs(x = "PCA-1", y = "PCA-2", fill = "Subjects")
    expect_no_warning(built <- ggplot_build(p))
    expect_equal(built$data[[1]]$xmin, original_scores$Dim.1$min)
    expect_equal(built$data[[1]]$xmax, original_scores$Dim.1$max)
    expect_equal(built$data[[1]]$ymin, original_scores$Dim.2$min)
    expect_equal(built$data[[1]]$ymax, original_scores$Dim.2$max)
    output <- tempfile(fileext = ".pdf")
    expect_no_warning(ggsave(output, p, device = grDevices::pdf,
                            width = 7, height = 5))
    expect_gt(file.info(output)$size, 0)
    unlink(output)
  }
  verify_pca_recipe()
})
