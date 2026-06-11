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

test_that("plot_bland_altman returns a ggplot object", {
  # 1. SETUP
  aligned_data <- dplyr::tibble(
    model_count = c(10, 1, 5, 0, 0),
    truth_count = c(12, 0, 0, 2, 8)
  )

  # 2. EXECUTION & ASSERTION
  expect_silent({
    p <- plot_bland_altman(aligned_data)
  })
  expect_s3_class(p, "ggplot")
})

test_that("plot_roc_curve returns a ggplot object", {
  # 1. SETUP
  roc_data <- dplyr::tibble(
    score = c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2),
    status = c("TP", "TP", "FP", "TP", "FP", "FP", "TP", "FP")
  )

  # 2. EXECUTION & ASSERTION
  expect_silent({
    p <- plot_roc_curve(roc_data)
  })
  expect_s3_class(p, "ggplot")
  # Check that it can be built without errors
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("plot_pr_curve returns a ggplot object", {
  # 1. SETUP
  pr_data <- dplyr::tibble(
    score = c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2),
    status = c("TP", "TP", "FP", "TP", "FP", "FP", "TP", "FP")
  )

  # 2. EXECUTION & ASSERTION
  expect_silent({
    p <- plot_pr_curve(pr_data)
  })
  expect_s3_class(p, "ggplot")
  # Check that it can be built without errors
  expect_silent(ggplot2::ggplot_build(p))
})