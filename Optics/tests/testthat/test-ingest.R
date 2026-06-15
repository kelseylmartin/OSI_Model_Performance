test_that("read_kwcoco returns a valid OpticsDetections S4 object", {
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
  result_obj <- read_kwcoco(json_path)
  
  # 3. ASSERTION: Check the S4 object and its slots
  expect_s4_class(result_obj, "OpticsDetections")
  expect_equal(result_obj@source_file, json_path)
  expect_equal(result_obj@ingest_format, "kwcoco")
  
  # Check the data within the @data slot
  result_data <- result_obj@data
  expect_true(is.data.frame(result_data))
  expect_equal(nrow(result_data), 1)
  
  expected_cols <- c(
    "video_id", "image_id", "frame_index", "annotation_id", "track_id",
    "category_id", "category_name", "bbox_x", "bbox_y", "bbox_width",
    "bbox_height", "score", "area"
  )
  expect_true(all(expected_cols %in% names(result_data)))
  
  expect_equal(result_data$video_id, "video_1")
  expect_equal(result_data$image_id, "frame_001.jpg")
  expect_equal(result_data$category_name, "Gadus morhua")
  expect_equal(result_data$score, 0.95)
  expect_equal(result_data$bbox_x, 10)
  
  # 4. TEARDOWN: Clean up the temporary file
  unlink(json_path)
})

test_that("read_viame_csv returns a valid OpticsDetections S4 object", {
  # 1. SETUP: Create a temporary, valid VIAME CSV file
  viame_content <- c(
    "# 1: Detection or Track-id",
    "# 2: Video or Image Identifier",
    "1,video_a.mp4,10,100,200,150,250,0.98,1.0,Gadus morhua,1.0"
  )
  csv_path <- tempfile(fileext = ".csv")
  writeLines(viame_content, csv_path)
  
  # 2. EXECUTION: Run the function
  result_obj <- read_viame_csv(csv_path)
  
  # 3. ASSERTION: Check the S4 object and its slots
  expect_s4_class(result_obj, "OpticsDetections")
  expect_equal(result_obj@source_file, csv_path)
  expect_equal(result_obj@ingest_format, "viame_csv")
  
  # Check the data within the @data slot
  result_data <- result_obj@data
  expect_true(is.data.frame(result_data))
  expect_equal(nrow(result_data), 1)
  
  expected_cols <- c(
    "video_id", "image_id", "frame_index", "annotation_id",
    "category_name", "bbox_x", "bbox_y", "bbox_width", "bbox_height", "score"
  )
  expect_true(all(expected_cols %in% names(result_data)))
  
  expect_equal(result_data$video_id, tools::file_path_sans_ext(basename(csv_path)))
  expect_equal(result_data$frame_index, 10)
  expect_equal(result_data$category_name, "Gadus morhua")
  expect_equal(result_data$score, 0.98)
  expect_equal(result_data$bbox_width, 50)
  expect_equal(result_data$bbox_height, 50)
  
  # 4. TEARDOWN
  unlink(csv_path)
})

test_that("read_kwcoco handles empty or invalid files gracefully", {
  # Test with a non-existent file
  non_existent_path <- "non_existent_file.json"
  expect_warning(result_obj <- read_kwcoco(non_existent_path))
  expect_s4_class(result_obj, "OpticsDetections")
  expect_equal(nrow(result_obj@data), 0)
  
  # Test with an empty JSON file
  empty_json_path <- tempfile(fileext = ".json")
  jsonlite::write_json(list(), empty_json_path)
  expect_warning(result_obj_empty <- read_kwcoco(empty_json_path))
  expect_s4_class(result_obj_empty, "OpticsDetections")
  expect_equal(nrow(result_obj_empty@data), 0)
  unlink(empty_json_path)
})

test_that("read_viame_csv handles empty or invalid files gracefully", {
  # Test with a non-existent file
  non_existent_path <- "non_existent_file.csv"
  expect_warning(result_obj <- read_viame_csv(non_existent_path))
  expect_s4_class(result_obj, "OpticsDetections")
  expect_equal(nrow(result_obj@data), 0)
  
  # Test with an empty CSV file
  empty_csv_path <- tempfile(fileext = ".csv")
  file.create(empty_csv_path)
  expect_warning(result_obj_empty <- read_viame_csv(empty_csv_path))
  expect_s4_class(result_obj_empty, "OpticsDetections")
  expect_equal(nrow(result_obj_empty@data), 0)
  unlink(empty_csv_path)
})
