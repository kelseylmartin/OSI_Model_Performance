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

test_that("plot_multiclass_confusion_matrix returns a ggplot object", {
  # 1. Create sample confusion matrix data
  confusion_data <- dplyr::tribble(
    ~Truth,     ~Prediction,          ~n,
    "SpeciesA", "SpeciesA",           10L,
    "SpeciesB", "SpeciesA",           2L,
    "SpeciesA", "FN (No Prediction)", 1L
  )

  # 2. Call the plotting function
  p <- plot_multiclass_confusion_matrix(confusion_data, title = "Test Plot")

  # 3. Assert that the output is a ggplot object
  testthat::expect_s3_class(p, "ggplot")
})

test_that("plot_multiclass_confusion_matrix throws error with incorrect columns", {
  # 1. Create data with missing columns
  bad_data <- dplyr::tribble(~True_Species, ~Predicted_Species, ~count, "SpeciesA", "SpeciesA", 10L)

  # 2. Assert that the function stops with an informative error
  testthat::expect_error(plot_multiclass_confusion_matrix(bad_data))
})

test_that("plot_performance_by_threshold works correctly", {
  # 1. SETUP
  perf_summary <- dplyr::tibble(
    threshold = rep(seq(0.1, 0.5, 0.1), 2),
    precision = runif(10, 0.5, 1),
    recall = runif(10, 0.5, 1),
    f1_score = runif(10, 0.5, 1),
    model_name = rep(c("Model A", "Model B"), each = 5)
  )

  # 2. EXECUTION
  p <- plot_performance_by_threshold(perf_summary, model_col = model_name)

  # 3. ASSERTION
  expect_s3_class(p, "ggplot")

  # Check that it creates facets for the different models
  expect_true("FacetWrap" %in% class(p$facet))

  # Check that the underlying data is pivoted correctly
  # ggplot maps the 'metric' column to 'colour', so we test for that.
  p_data <- ggplot2::ggplot_build(p)$data[[1]]
  expect_true("colour" %in% names(p_data))
  expect_equal(length(unique(p_data$colour)), 3) # One color for each metric
})