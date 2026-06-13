test_that("calculate_maxn works correctly", {
  # 1. SETUP
  sample_df <- dplyr::tibble(
    video_id = c(rep("v1", 4), rep("v2", 3)),
    frame_index = c(1, 1, 2, 2, 1, 1, 1),
    category_name = c("A", "A", "A", "B", "A", "A", "A"),
    model_name = "TestModel"
  )

  # 2. EXECUTION
  maxn_results <- calculate_maxn(sample_df)
  maxn_grouped <- calculate_maxn(sample_df, group_cols = "model_name")

  # 3. ASSERTION
  expect_equal(nrow(maxn_results), 3)
  # v1, FishA should have MaxN of 2
  expect_equal(maxn_results$maxn[maxn_results$video_id == "v1" & maxn_results$category_name == "A"], 2)
  # v2, FishA should have MaxN of 3
  expect_equal(maxn_results$maxn[maxn_results$video_id == "v2" & maxn_results$category_name == "A"], 3)
  # Check grouped calculation
  expect_true("model_name" %in% names(maxn_grouped))
})

test_that("calculate_density works correctly", {
  # 1. SETUP
  count_data <- dplyr::tibble(
    site = c("A", "B"),
    count = c(10, 25),
    area_m2 = c(5, 10)
  )

  # 2. EXECUTION
  # Test with a single area value
  density_fixed_area <- calculate_density(count_data, count_col = count, area = 50)
  # Test with an area column
  density_col_area <- calculate_density(count_data, count_col = count, area_col = area_m2)

  # 3. ASSERTION
  expect_true("density" %in% names(density_fixed_area))
  expect_equal(density_fixed_area$density, c(10/50, 25/50))

  expect_true("density" %in% names(density_col_area))
  expect_equal(density_col_area$density, c(10/5, 25/10))

  # Test error handling
  expect_error(
    calculate_density(count_data, count_col = count, area = 50, area_col = area_m2),
    "Please provide 'area' or 'area_col', but not both."
  )
})