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
  expect_s3_class(result_df, "tbl_df")
  expect_equal(nrow(result_df), 1)

  expected_cols <- c(
    "video_id", "image_id", "frame_index", "annotation_id", "track_id",
    "category_id", "category_name", "bbox_x", "bbox_y", "bbox_width",
    "bbox_height", "score", "area"
  )
  expect_true(all(expected_cols %in% names(result_df)))

  expect_equal(result_df$video_id, "video_1")
  expect_equal(result_df$image_id, "frame_001.jpg")
  expect_equal(result_df$category_name, "Gadus morhua")
  expect_equal(result_df$score, 0.95)
  expect_equal(result_df$bbox_x, 10)

  # 4. TEARDOWN: Clean up the temporary file
  unlink(json_path)
})

test_that("read_viame_csv successfully ingests and standardizes a valid file", {
  # 1. SETUP: Create a temporary, valid VIAME CSV file
  viame_content <- c(
    "# 1: Detection or Track-id",
    "# 2: Video or Image Identifier",
    "1,video_a.mp4,10,100,200,150,250,0.98,1.0,Gadus morhua,1.0"
  )
  csv_path <- tempfile(fileext = ".csv")
  writeLines(viame_content, csv_path)

  # 2. EXECUTION: Run the function
  result_df <- read_viame_csv(csv_path)

  # 3. ASSERTION: Check the output
  expect_s3_class(result_df, "tbl_df")
  expect_equal(nrow(result_df), 1)

  expected_cols <- c(
    "video_id", "image_id", "frame_index", "annotation_id",
    "category_name", "bbox_x", "bbox_y", "bbox_width", "bbox_height", "score"
  )
  expect_true(all(expected_cols %in% names(result_df)))

  expect_equal(result_df$video_id, tools::file_path_sans_ext(basename(csv_path)))
  expect_equal(result_df$frame_index, 10)
  expect_equal(result_df$category_name, "Gadus morhua")
  expect_equal(result_df$score, 0.98)
  expect_equal(result_df$bbox_width, 50)
  expect_equal(result_df$bbox_height, 50)

  # 4. TEARDOWN
  unlink(csv_path)
})
