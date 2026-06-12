test_that("plot_pr_curve works for a single model", {
  # 1. SETUP
  pr_data <- dplyr::tibble(
    score = c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2),
    status = c("TP", "TP", "FP", "TP", "FP", "FP", "TP", "FP")
  )

  # 2. EXECUTION
  p <- plot_pr_curve(pr_data)

  # 3. ASSERTION
  expect_s3_class(p, "ggplot")
  # Check that the plot title is correct
  expect_equal(p$labels$title, "Precision-Recall Curve")
})

test_that("plot_pr_curve handles multiple models correctly", {
  # 1. SETUP
  pr_data_multi <- dplyr::tibble(
    score = c(0.9, 0.8, 0.7, 0.6, 0.55, 0.45),
    status = c("TP", "FP", "TP", "FP", "TP", "FP"),
    model_identifier = rep(c("Model A", "Model B"), each = 3)
  )

  # 2. EXECUTION
  p <- plot_pr_curve(pr_data_multi, model_col = model_identifier)

  # 3. ASSERTION
  expect_s3_class(p, "ggplot")
  # Check that there are two distinct groups in the plot data for the two models
  expect_equal(length(unique(p$data$legend_label)), 2)
  # Check that the legend labels are formatted correctly with model name and AUC
  expect_true(any(grepl("Model A \\(AUC-PR = .+", p$data$legend_label)))
  expect_true(any(grepl("Model B \\(AUC-PR = .+", p$data$legend_label)))
})

test_that("plot_confusion_matrix works correctly", {
  # 1. SETUP
  metrics <- dplyr::tibble(tp = 10, fp = 2, fn = 3, tn = 85)

  # 2. EXECUTION
  p <- plot_confusion_matrix(metrics, title = "Test CM")

  # 3. ASSERTION
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Test CM")

  # Check the underlying data for the plot tiles
  # The data should have 4 rows, one for each quadrant of the matrix
  expect_equal(nrow(p$data), 4)

  # Check that the values from the input metrics are correctly placed in the plot data
  # TP: Truth=Positive, Prediction=Positive
  expect_equal(p$data$N[p$data$Truth == "Positive" & p$data$Prediction == "Positive"], metrics$tp)
  # FN: Truth=Positive, Prediction=Negative
  expect_equal(p$data$N[p$data$Truth == "Positive" & p$data$Prediction == "Negative"], metrics$fn)
  # FP: Truth=Negative, Prediction=Positive
  expect_equal(p$data$N[p$data$Truth == "Negative" & p$data$Prediction == "Positive"], metrics$fp)
  # TN: Truth=Negative, Prediction=Negative
  expect_equal(p$data$N[p$data$Truth == "Negative" & p$data$Prediction == "Negative"], metrics$tn)
})

test_that("plot_roc_curve works correctly", {
  # 1. SETUP
  roc_data <- dplyr::tibble(
    score = c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2),
    status = c("TP", "TP", "FP", "TP", "FP", "FP", "TP", "FP")
  )

  # 2. EXECUTION
  p <- plot_roc_curve(roc_data, title = "Test ROC")

  # 3. ASSERTION
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Test ROC")

  # Check that the plot contains the key layers:
  # The ROC curve itself (GeomPath from ggroc), the diagonal line, and the AUC text
  geoms <- sapply(p$layers, function(x) class(x$geom)[1])
  expect_true("GeomLine" %in% geoms)
  expect_true("GeomAbline" %in% geoms)
  expect_true("GeomText" %in% geoms)

  # Build the plot and check the data for the text layer
  plot_build <- ggplot2::ggplot_build(p)
  text_layer_data <- plot_build$data[[3]]
  expect_true(grepl("AUC =", text_layer_data$label))
})