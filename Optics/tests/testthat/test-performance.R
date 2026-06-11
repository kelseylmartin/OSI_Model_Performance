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