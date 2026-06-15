test_that("calculate_maxn method for OpticsDetections works correctly", {
  # 1. SETUP
  sample_df <- dplyr::tibble(
    video_id = c(rep("v1", 4), rep("v2", 3)),
    image_id = "test_image",
    frame_index = c(1, 1, 2, 2, 1, 1, 1),
    category_name = c("A", "A", "A", "B", "A", "A", "A"),
    model_name = "TestModel",
    # Add columns required by the OpticsDetections validator
    annotation_id = 1:7,
    bbox_x = 0, bbox_y = 0, bbox_width = 0, bbox_height = 0, score = 0
  )
  detections_obj <- OpticsDetections(
    data = sample_df,
    source_file = "test.csv",
    ingest_format = "test"
  )

  # 2. EXECUTION
  maxn_results <- calculate_maxn(detections_obj)
  maxn_grouped <- calculate_maxn(detections_obj, group_cols = "model_name")

  # 3. ASSERTION
  expect_equal(nrow(maxn_results), 3)
  # v1, FishA should have MaxN of 2
  expect_equal(maxn_results$maxn[maxn_results$video_id == "v1" & maxn_results$category_name == "A"], 2)
  # v2, FishA should have MaxN of 3
  expect_equal(maxn_results$maxn[maxn_results$video_id == "v2" & maxn_results$category_name == "A"], 3)
  # Check grouped calculation
  expect_true("model_name" %in% names(maxn_grouped))
})

test_that("calculate_frame_abundance method for OpticsDetections works", {
  # 1. SETUP
  sample_df <- dplyr::tibble(
    video_id = c(rep("v1", 3)),
    image_id = "test_image",
    frame_index = c(1, 1, 2),
    category_name = c("A", "A", "B"),
    # Add columns required by the OpticsDetections validator
    annotation_id = 1:3,
    bbox_x = 0, bbox_y = 0, bbox_width = 0, bbox_height = 0, score = 0
  )
  detections_obj <- OpticsDetections(
    data = sample_df,
    source_file = "test.csv",
    ingest_format = "test"
  )

  # 2. EXECUTION
  abundance_results <- calculate_frame_abundance(detections_obj)

  # 3. ASSERTION
  expect_equal(nrow(abundance_results), 2)
  expect_equal(abundance_results$abundance[abundance_results$frame_index == 1], 2)
  expect_equal(abundance_results$abundance[abundance_results$frame_index == 2], 1)
})


test_that("calculate_density (S3 utility) works correctly", {
  # 1. SETUP
  count_data <- dplyr::tibble(
    site = c("A", "B"),
    count = c(10, 25),
    area_m2 = c(5, 10)
  )

  # 2. EXECUTION
  density_fixed_area <- calculate_density(count_data, count_col = count, area = 50)
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
