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
  # Check new metrics
  expect_equal(metrics$accuracy, (1 + 95) / 100)
  expect_equal(metrics$fpr, 2 / (2 + 95)) # FP / (FP + TN)
  expect_equal(metrics$fnr, 2 / (2 + 1)) # FN / (FN + TP)
  expect_equal(metrics$false_positive_ratio, 2 / (1 + 2 + 95)) # FP / (TP + FN + TN)
  expect_equal(metrics$false_negative_ratio, 2 / (1 + 2 + 95)) # FN / (TP + FP + TN)
  
  # MCC = (TP*TN - FP*FN) / sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
  # MCC = (1*95 - 2*2) / sqrt((1+2)*(1+2)*(95+2)*(95+2))
  # MCC = 91 / sqrt(3 * 3 * 97 * 97) = 91 / (3 * 97) = 91 / 291
  expect_equal(metrics$mcc, 91 / 291)
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

test_that("calculate_confusion_matrix works correctly", {
  # 1. Create sample aligned data to simulate different scenarios
  aligned_data <- dplyr::tibble(
    Deployment = c(
      "Dep1", "Dep1", # Scenario: Manual: A, Model: A (Correctly identifies A)
      "Dep2", "Dep2", # Scenario: Manual: B, Model: C (Misclassifies B as C)
      "Dep3",         # Scenario: Manual: D, Model: nothing (False Negative for D)
      "Dep4"          # Scenario: Manual: nothing, Model: E (False Positive for E)
    ),
    Species = c(
      "SpeciesA", "SpeciesA",
      "SpeciesB", "SpeciesC",
      "SpeciesD",
      "SpeciesE"
    ),
    Manual = c(1, 0, 1, 0, 1, 0),
    VIAME_MaxN = c(0, 1, 0, 1, 0, 1)
  )

  # 2. Run the function, explicitly passing the column names
  confusion_result <- calculate_confusion_matrix(
    aligned_data,
    group_vars = "Deployment",
    species_col = Species,
    model_col = VIAME_MaxN,
    truth_col = Manual
  )

  # 3. Define the expected output
  expected_result <- dplyr::tribble(
    ~Truth,     ~Prediction,          ~n,
    "SpeciesA", "SpeciesA",           1L,
    "SpeciesB", "SpeciesC",           1L,
    "SpeciesD", "FN (No Prediction)", 1L
  ) %>% dplyr::arrange(Truth, Prediction)

  # 4. Assert that the result matches the expectation
  expect_equal(dplyr::arrange(confusion_result, Truth, Prediction), expected_result)
})

test_that("analyze_reviewer_effort calculates metrics correctly", {
  # 1. SETUP: Create sample raw and validated data
  raw <- dplyr::tibble(
    Deployment = "D1",
    TrackID = 1:6,
    Species = c("seal", "seal", "rock", "seal", "glare", "seal")
  )
  # Reviewer keeps tracks 1, 2, 4 as "seal".
  # Reviewer reclassifies track 6 from "seal" to "beluga".
  # Reviewer deletes tracks 3 ("rock") and 5 ("glare").
  validated <- dplyr::tibble(
    Deployment = "D1",
    TrackID = c(1, 2, 4, 6),
    Species = c("seal", "seal", "seal", "beluga")
  )

  # 2. EXECUTION
  effort_summary <- analyze_reviewer_effort(raw, validated, group_vars = "Deployment")

  # 3. ASSERTION
  expect_equal(nrow(effort_summary), 1)
  expect_equal(effort_summary$n_raw, 6)
  expect_equal(effort_summary$n_validated, 4)
  expect_equal(effort_summary$n_deleted, 2)
  expect_equal(effort_summary$avg_raw_per_validated, 6 / 4)
  # There is one reclassification: track 6 from seal to beluga.
  expect_equal(effort_summary$n_reclassified, 1)

  # Test with no reclassifications
  validated_no_reclass <- dplyr::filter(validated, TrackID != 6)
  effort_no_reclass <- analyze_reviewer_effort(raw, validated_no_reclass, group_vars = "Deployment")
  expect_equal(effort_no_reclass$n_reclassified, 0)
})

test_that("get_disagreement_report works correctly", {
  # 1. SETUP
  aligned_data <- dplyr::tibble(
    Deployment = c("D1", "D1", "D2", "D2", "D3", "D3", "D3"),
    Species = c("Seal", "Rock", "Seal", "Glare", "Seal", "Seal", "Fish"),
    model_count = c(1, 1, 0, 1, 1, 0, 1),
    truth_count = c(1, 0, 1, 0, 0, 1, 0)
  )
  # D1: 1 FP (Rock) -> total 1 error
  # D2: 1 FN (Seal), 1 FP (Glare) -> total 2 errors
  # D3: 1 FP (Seal), 1 FN (Seal), 1 FP (Fish) -> total 3 errors

  # 2. EXECUTION
  report <- get_disagreement_report(aligned_data, group_vars = "Deployment")

  # 3. ASSERTION
  expect_equal(nrow(report), 3)
  # Check that it's ranked correctly by total disagreement
  expect_equal(report$Deployment, c("D3", "D2", "D1"))

  # Check the values for the top offender (D3)
  d3_report <- report %>% dplyr::filter(Deployment == "D3")
  expect_equal(d3_report$false_positives, 2)
  expect_equal(d3_report$false_negatives, 1)
  expect_equal(d3_report$total_disagreement, 3)
})

test_that("analyze_performance_drivers fits a model correctly", {
  # 1. SETUP: Create more complex aligned data for modeling
  aligned_data <- dplyr::tibble(
    Deployment = rep(c("D1", "D2", "D3", "D4"), each = 3),
    Species = rep(c("Seal", "Rock", "Fish"), 4),
    # D1: Low complexity, low error
    # D2: High species richness, higher error
    # D3: High abundance, higher error
    # D4: Low complexity, high error (for model variability)
    truth_count = c(2, 0, 0,  5, 2, 1,  20, 0, 0,  1, 0, 0),
    model_count = c(2, 1, 0,  3, 3, 0,  15, 1, 1,  5, 1, 0)
  )
  # Expected predictors & errors by Deployment:
  # D1: n_spec=1, tot_ind=2,  err=abs(2-2)+abs(0-1)+abs(0-0) = 1
  # D2: n_spec=3, tot_ind=8,  err=abs(3-5)+abs(3-2)+abs(0-1) = 4
  # D3: n_spec=1, tot_ind=20, err=abs(15-20)+abs(1-0)+abs(1-0) = 7
  # D4: n_spec=1, tot_ind=1,  err=abs(5-1)+abs(1-0)+abs(0-0) = 5

  # 2. EXECUTION
  # Suppress convergence warnings that can occur with small sample sizes in tests
  suppressWarnings({
    model_fit <- analyze_performance_drivers(aligned_data, group_vars = "Deployment")
  })

  # 3. ASSERTION
  # Check that the output is a valid glmer model object
  expect_s4_class(model_fit, "glmerMod")

  # Check that the model fixed effects are what we expect
  model_coefs <- lme4::fixef(model_fit)
  expect_true("n_species_truth" %in% names(model_coefs))
  expect_true("total_individuals_truth" %in% names(model_coefs))

  # Check that the random effects are what we expect
  random_effects <- names(lme4::ranef(model_fit))
  expect_true("Species" %in% random_effects)
  expect_true("Deployment" %in% random_effects)
})