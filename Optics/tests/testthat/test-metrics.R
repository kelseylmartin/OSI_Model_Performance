test_that("calculate_maxn works correctly for basic case", {
  # 1. SETUP: Create a sample standardized data frame
  sample_df <- dplyr::tibble(
    video_id = c(rep("video1", 5), rep("video2", 3)),
    frame_index = c(1, 1, 2, 2, 2, 1, 1, 1),
    category_name = c("FishA", "FishA", "FishA", "FishB", "FishA", "FishA", "FishA", "FishA"),
    annotation_id = 1:8
  )

  # 2. EXECUTION
  result <- calculate_maxn(sample_df)

  # 3. ASSERTION
  # Expected: video1/FishA has 2 in frame 1, 2 in frame 2 -> MaxN = 2
  #           video1/FishB has 1 in frame 2 -> MaxN = 1
  #           video2/FishA has 3 in frame 1 -> MaxN = 3
  expect_equal(nrow(result), 3)
  expect_true(all(c("video_id", "category_name", "maxn") %in% names(result)))

  # Check specific values
  expect_equal(result$maxn[result$video_id == "video1" & result$category_name == "FishA"], 2)
  expect_equal(result$maxn[result$video_id == "video1" & result$category_name == "FishB"], 1)
  expect_equal(result$maxn[result$video_id == "video2" & result$category_name == "FishA"], 3)
})

test_that("calculate_maxn works with additional group_cols", {
  # 1. SETUP
  sample_df <- dplyr::tibble(
    video_id = "v1",
    frame_index = c(1, 1, 2, 2),
    category_name = "FishA",
    confidence = c(0.8, 0.8, 0.9, 0.9), # Extra column to group by
    annotation_id = 1:4
  )

  # 2. EXECUTION
  result <- calculate_maxn(sample_df, group_cols = "confidence")

  # 3. ASSERTION
  expect_equal(nrow(result), 2) # Should have one row for each confidence group
  expect_equal(result$maxn[result$confidence == 0.8], 2)
  expect_equal(result$maxn[result$confidence == 0.9], 2)
})

test_that("calculate_frame_abundance works correctly", {
  # 1. SETUP
  sample_df <- dplyr::tibble(
    video_id = "v1",
    frame_index = c(1, 1, 2, 2, 2),
    category_name = c("FishA", "FishA", "FishA", "FishB", "FishA"),
    annotation_id = 1:5
  )

  # 2. EXECUTION
  result <- calculate_frame_abundance(sample_df)

  # 3. ASSERTION
  # Expected: v1/frame1/FishA -> 2
  #           v1/frame2/FishA -> 2
  #           v1/frame2/FishB -> 1
  expect_equal(nrow(result), 3)
  expect_true(all(c("video_id", "frame_index", "category_name", "abundance") %in% names(result)))

  # Check specific values
  expect_equal(result$abundance[result$frame_index == 1 & result$category_name == "FishA"], 2)
  expect_equal(result$abundance[result$frame_index == 2 & result$category_name == "FishA"], 2)
  expect_equal(result$abundance[result$frame_index == 2 & result$category_name == "FishB"], 1)
})