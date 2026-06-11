test_that("read_kwcoco successfully ingests and flattens a valid file", {
  # 1. SETUP: Create a temporary, valid KWCOCO JSON file
  dummy_kwcoco <- list(
    videos = list(list(id = 1, name = "video_1")),
    images = list(
      list(id = 101, video_id = 1, file_name = "frame_001.jpg", frame_index = 1),
      list(id = 102, video_id = 1, file_name = "frame_002.jpg", frame_index = 2)
    ),
    annotations = list(
      list(id = 1, image_id = 101, category_id = 1, track_id = 5,
           bbox = c(10, 20, 30, 40), score = 0.95, area = 1200)
    ),
    categories = list(list(id = 1, name = "Gadus morhua"))
  )
  json_path <- tempfile(fileext = ".json")
  jsonlite::write_json(dummy_kwcoco, json_path, auto_unbox = TRUE)
  
  # 2. EXECUTION: Run the function
  result_df <- read_kwcoco(json_path)
  
  # 3. ASSERTION: Check the output
  
  # Check that it's a tibble with the correct dimensions
  expect_s3_class(result_df, "tbl_df")
  expect_equal(nrow(result_df), 1)
  
  # Check for the presence of all expected columns from our internal schema
  expected_cols <- c(
    "video_id", "image_id", "frame_index", "annotation_id", "track_id",
    "category_id", "category_name", "bbox_x", "bbox_y", "bbox_width",
    "bbox_height", "score", "area"
  )
  expect_true(all(expected_cols %in% names(result_df)))
  
  # Check a few values to ensure data integrity
  expect_equal(result_df$video_id, "video_1")
  expect_equal(result_df$image_id, "frame_001.jpg")
  expect_equal(result_df$category_name, "Gadus morhua")
  expect_equal(result_df$score, 0.95)
  expect_equal(result_df$bbox_x, 10)
  
  # 4. TEARDOWN: Clean up the temporary file
  unlink(json_path)
})

test_that("read_kwcoco handles missing files gracefully", {
  # Expect a warning for a non-existent file
  expect_warning(
    result <- read_kwcoco("non_existent_file.json"),
    "File does not exist"
  )
  # Expect an empty tibble as the return value
  expect_true(is.data.frame(result) && nrow(result) == 0)
})

test_that("read_kwcoco handles malformed JSON files", {
  # 1. SETUP: Create a malformed JSON
  bad_json_path <- tempfile(fileext = ".json")
  writeLines("{'videos': [{'id': 1}]", bad_json_path) # Invalid JSON with single quotes
  
  # 2. EXECUTION & ASSERTION: Expect a warning and an empty tibble
  expect_warning(
    result <- read_kwcoco(bad_json_path),
    "Failed to parse JSON file"
  )
  expect_true(is.data.frame(result) && nrow(result) == 0)
  
  # 3. TEARDOWN
  unlink(bad_json_path)
})

test_that("read_kwcoco handles files with no annotations", {
  # 1. SETUP: Create a valid KWCOCO file but with an empty annotations list
  empty_kwcoco <- list(
    videos = list(list(id = 1, name = "video_1")),
    images = list(list(id = 101, video_id = 1, file_name = "frame_001.jpg")),
    annotations = list(), # Empty list
    categories = list(list(id = 1, name = "Gadus morhua"))
  )
  json_path <- tempfile(fileext = ".json")
  jsonlite::write_json(empty_kwcoco, json_path, auto_unbox = TRUE)
  
  # 2. EXECUTION & ASSERTION
  expect_warning(
    result <- read_kwcoco(json_path),
    "KWCOCO file is empty or contains no annotations"
  )
  expect_true(is.data.frame(result) && nrow(result) == 0)
  
  # 3. TEARDOWN
  unlink(json_path)
})

