test_that("plot_counts_scatterplot returns a ggplot object", {
  # 1. SETUP
  aligned_data <- dplyr::tibble(
    model_count = c(10, 1, 5, 0, 0),
    truth_count = c(12, 0, 0, 2, 8)
  )

  # 2. EXECUTION
  p <- plot_counts_scatterplot(aligned_data)

  # 3. ASSERTION
  # Check that the output is a ggplot object
  expect_s3_class(p, "ggplot")
  # Check that it can be built without errors
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("plot_confusion_matrix returns a ggplot object", {
  # 1. SETUP
  metrics_data <- dplyr::tibble(
    tp = 50, fp = 5, fn = 10, tn = 100
  )

  # 2. EXECUTION
  p <- plot_confusion_matrix(metrics_data)

  # 3. ASSERTION
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})