test_that("calculate_binary_metrics works for ungrouped data", {
  # 1. SETUP
  aligned_data <- dplyr::tibble(
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
  expect_equal(metrics$tn, 95)
})

test_that("calculate_binary_metrics works for grouped data", {
  # SETUP
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
  
  expect_equal(fish_b_metrics$tp, 1)
  expect_equal(fish_b_metrics$fp, 0)
  expect_equal(fish_b_metrics$fn, 0)
})

test_that("summarize_performance_by_threshold works correctly", {
  # 1. SETUP
  model_dets_df <- dplyr::tibble(
    video_id = "v1", image_id = "img1", frame_index = c(1, 1, 2),
    annotation_id = 1:3, category_name = c("FishA", "FishA", "FishB"),
    score = c(0.95, 0.85, 0.82), bbox_x = 0, bbox_y = 0, bbox_width = 0, bbox_height = 0
  )
  truth_dets_df <- dplyr::tibble(
    video_id = "v1", image_id = "img1", frame_index = c(1, 3),
    annotation_id = 4:5, category_name = c("FishA", "FishC"),
    score = 1.0, bbox_x = 0, bbox_y = 0, bbox_width = 0, bbox_height = 0
  )
  
  model_detections <- OpticsDetections(model_dets_df, "model.csv", "test")
  truth_detections <- OpticsDetections(truth_dets_df, "truth.csv", "test")
  
  # 2. EXECUTION
  summary_df <- summarize_performance_by_threshold(
    model_detections = model_detections,
    truth_detections = truth_detections,
    by = c("video_id", "category_name"),
    thresholds = c(0.8, 0.9)
  )
  
  # 3. ASSERTION
  expect_equal(nrow(summary_df), 2)
  metrics_at_08 <- summary_df[summary_df$threshold == 0.8, ]
  expect_equal(metrics_at_08$tp, 1)
  expect_equal(metrics_at_08$fp, 1)
  expect_equal(metrics_at_08$fn, 1)
})

test_that("classify_detections works correctly", {
  # 1. SETUP
  raw <- dplyr::tibble(
    detection_id = 1:5,
    score = c(0.9, 0.8, 0.7, 0.6, 0.5),
    category = "seal"
  )
  validated <- dplyr::tibble(
    detection_id = c(1, 3, 5)
  )
  
  # 2. EXECUTION
  classified <- classify_detections(raw, validated, detection_id = detection_id)
  
  # 3. ASSERTION
  expect_equal(nrow(classified), 5)
  expect_true("status" %in% names(classified))
  expect_equal(sort(classified$detection_id[classified$status == "TP"]), c(1, 3, 5))
  expect_equal(sort(classified$detection_id[classified$status == "FP"]), c(2, 4))
})

# The remaining tests use the S4 data.frame methods and should not need
# significant changes, as they operate on generic data frames.
