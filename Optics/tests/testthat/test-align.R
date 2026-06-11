test_that("align_counts correctly joins model and truth data", {
  # 1. SETUP
  model_df <- dplyr::tibble(
    video_id = c("v1", "v1", "v2"),
    category_name = c("FishA", "FishB", "FishA"),
    model_maxn = c(10, 1, 5)
  )

  truth_df <- dplyr::tibble(
    video_id = c("v1", "v1", "v3"),
    category_name = c("FishA", "FishC", "FishA"),
    truth_maxn = c(12, 2, 8)
  )

  # 2. EXECUTION
  aligned <- align_counts(
    model_counts = model_df,
    truth_counts = truth_df,
    by = c("video_id", "category_name"),
    model_col = model_maxn,
    truth_col = truth_maxn
  )

  # 3. ASSERTION
  # Expected 4 rows:
  # v1, FishA: match (10, 12)
  # v1, FishB: false positive (1, 0)
  # v2, FishA: false positive (5, 0)
  # v1, FishC: false negative (0, 2)
  # v3, FishA: false negative (0, 8)
  expect_equal(nrow(aligned), 5)
  expect_true(all(c("model_count", "truth_count") %in% names(aligned)))

  # Check a match, a false positive, and a false negative
  expect_equal(aligned$model_count[aligned$video_id == "v1" & aligned$category_name == "FishA"], 10)
  expect_equal(aligned$truth_count[aligned$video_id == "v1" & aligned$category_name == "FishA"], 12)
  expect_equal(aligned$model_count[aligned$video_id == "v1" & aligned$category_name == "FishB"], 1)
  expect_equal(aligned$truth_count[aligned$video_id == "v1" & aligned$category_name == "FishB"], 0)
  expect_equal(aligned$model_count[aligned$video_id == "v1" & aligned$category_name == "FishC"], 0)
  expect_equal(aligned$truth_count[aligned$video_id == "v1" & aligned$category_name == "FishC"], 2)
})