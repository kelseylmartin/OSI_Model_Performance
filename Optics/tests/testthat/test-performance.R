test_that("calculate_binary_metrics works for ungrouped data", {
  # 1. SETUP
  aligned_data <- dplyr::tibble(
    # TP: 1 case
    # FP: 2 cases
    # FN: 2 cases
    # TN should be 100 - (1+2+2) = 95
    model_count = c(10, 1, 5, 0, 0),
    truth_count = c(12, 0, 0, 2, 8)
  )
  total_obs <- 100

  # 2. EXECUTION
  metrics <- calculate_binary_metrics(aligned_data, total_comparisons = total_obs)

  # 3. ASSERTION
  expect_equal(metrics$tp, 1)
  expect_equal(metrics$fp, 2)
  expect_equal(metrics$fn, 2)

  expected_precision <- 1 / (1 + 2) # 0.333
  expected_recall <- 1 / (1 + 2)    # 0.333
  expected_f1 <- 2 * (expected_precision * expected_recall) / (expected_precision + expected_recall)

  expect_equal(metrics$precision, expected_precision)
  expect_equal(metrics$recall, expected_recall)
  expect_equal(metrics$f1_score, expected_f1)
  expect_equal(metrics$tn, 95)
})

test_that("calculate_binary_metrics works for grouped data", {
  # SETUP: FishA has 1 FP, 1 FN. FishB has 1 TP.
  aligned_data <- dplyr::tibble(
    category = c("FishA", "FishA", "FishB"),
    model_count = c(5, 0, 3),
    truth_count = c(0, 2, 4)
  )
  # EXECUTION
  metrics <- calculate_binary_metrics(aligned_data, group_vars = "category")

  # ASSERTION
  fish_a_metrics <- metrics[metrics$category == "FishA", ]
  fish_b_metrics <- metrics[metrics$category == "FishB", ]

  expect_equal(fish_a_metrics$tp, 0)
  expect_equal(fish_a_metrics$fp, 1)
  expect_equal(fish_a_metrics$fn, 1)
  expect_true(is.nan(fish_a_metrics$f1_score)) # F1 is NaN when precision or recall is 0

  expect_equal(fish_b_metrics$tp, 1)
  expect_equal(fish_b_metrics$fp, 0)
  expect_equal(fish_b_metrics$fn, 0)
  expect_equal(fish_b_metrics$f1_score, 1)
})

test_that("summarize_performance_by_threshold works correctly", {
  # 1. SETUP
  model_dets <- dplyr::tibble(
    video_id = "v1",
    frame_index = c(1, 1, 2),
    category_name = c("FishA", "FishA", "FishB"), # FishB is the FP
    score = c(0.95, 0.85, 0.82) # Changed 0.7 to 0.82 so it's > 0.8 threshold
  )
  truth_dets <- dplyr::tibble(
    video_id = "v1",
    frame_index = c(1, 3),
    category_name = c("FishA", "FishC")
  )

  # 2. EXECUTION
  summary_df <- summarize_performance_by_threshold(
    model_detections = model_dets,
    truth_detections = truth_dets,
    by = c("video_id", "category_name"),
    thresholds = c(0.8, 0.9)
  )

  # 3. ASSERTION
  expect_equal(nrow(summary_df), 2)
  expect_true("threshold" %in% names(summary_df))

  # At threshold 0.8: FishA is TP, FishB is FP. FishC is FN.
  metrics_at_08 <- summary_df %>% dplyr::filter(threshold == 0.8)
  expect_equal(metrics_at_08$tp, 1)
  expect_equal(metrics_at_08$fp, 1)
  expect_equal(metrics_at_08$fn, 1)
})

test_that("performance functions handle multi-model comparisons", {
  # 1. SETUP
  # Model A is perfect, Model B has one FP and one FN.
  model_dets_multi <- dplyr::tibble(
    video_id = "v1",
    frame_index = 1,
    category_name = c("FishA", "FishB", "FishA", "FishC"),
    score = 0.9,
    model_name = c("Model A", "Model A", "Model B", "Model B")
  )
  truth_dets_multi <- dplyr::tibble(
    video_id = "v1",
    frame_index = 1,
    category_name = c("FishA", "FishB", "FishA", "FishD"),
    model_name = c("Model A", "Model A", "Model B", "Model B")
  )

  # 2. EXECUTION
  # Align the data first, grouping by model
  model_counts <- calculate_maxn(model_dets_multi, group_cols = "model_name")
  truth_counts <- calculate_maxn(truth_dets_multi, group_cols = "model_name")
  aligned_multi <- align_counts(model_counts, truth_counts, by = c("video_id", "category_name", "model_name"))

  # Calculate metrics, grouped by model
  metrics <- calculate_binary_metrics(aligned_multi, group_vars = "model_name")

  # 3. ASSERTION
  expect_equal(nrow(metrics), 2)

  # Check Model A (perfect model)
  metrics_a <- metrics %>% dplyr::filter(model_name == "Model A")
  expect_equal(metrics_a$tp, 2)
  expect_equal(metrics_a$fp, 0)
  expect_equal(metrics_a$fn, 0)

  # Check Model B (imperfect model)
  metrics_b <- metrics %>% dplyr::filter(model_name == "Model B")
  expect_equal(metrics_b$tp, 1) # FishA
  expect_equal(metrics_b$fp, 1) # FishC
  expect_equal(metrics_b$fn, 1) # FishD
})

test_that("classify_detections works correctly", {
  # 1. SETUP
  raw <- dplyr::tibble(
    detection_id = 1:5,
    score = c(0.9, 0.8, 0.7, 0.6, 0.5),
    category = "seal"
  )
  # Human keeps detections 1, 3, 5. Detections 2 and 4 are False Positives.
  validated <- dplyr::tibble(
    detection_id = c(1, 3, 5)
  )

  # 2. EXECUTION
  classified <- classify_detections(raw, validated, detection_id = detection_id)

  # 3. ASSERTION
  expect_equal(nrow(classified), 5)
  expect_true("status" %in% names(classified))

  # Check status assignments
  tp_rows <- classified %>% dplyr::filter(status == "TP")
  fp_rows <- classified %>% dplyr::filter(status == "FP")

  expect_equal(nrow(tp_rows), 3)
  expect_equal(nrow(fp_rows), 2)
  expect_equal(sort(tp_rows$detection_id), c(1, 3, 5))
  expect_equal(sort(fp_rows$detection_id), c(2, 4))

  # Test error handling for missing column
  expect_error(
    classify_detections(raw, validated, detection_id = wrong_id)
  )
})