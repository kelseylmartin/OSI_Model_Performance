describe("Ice Seals custom ingestion and analysis", {

  test_that("ingest_ice_seals_csv reads and combines data correctly", {
    # Get paths to the example files
    left_file <- system.file("extdata", "AKFSC", "ice_seals_2025_fl223_L_ir_detections_validated.csv", package = "Optics")
    center_file <- system.file("extdata", "AKFSC", "ice_seals_2025_fl223_C_ir_detections_validated.csv", package = "Optics")
    right_file <- system.file("extdata", "AKFSC", "ice_seals_2025_fl223_R_ir_detections_validated.csv", package = "Optics")

    # Ingest the data
    ice_seals_detections <- ingest_ice_seals_csv(c(left_file, center_file, right_file))

    # Check that the output is an OpticsDetections object
    expect_s4_class(ice_seals_detections, "OpticsDetections")

    # Check that the data slot is not empty
    expect_true(nrow(ice_seals_detections@data) > 0)

    # Check for required columns
    required_cols <- c("video_id", "image_id", "frame_index", "annotation_id",
                       "category_name", "bbox_x", "bbox_y", "bbox_width",
                       "bbox_height", "score")
    expect_true(all(required_cols %in% names(ice_seals_detections@data)))

    # Check that data from all three files is present
    expect_true(any(grepl("_L_", ice_seals_detections@data$image_id)))
    expect_true(any(grepl("_C_", ice_seals_detections@data$image_id)))
    expect_true(any(grepl("_R_", ice_seals_detections@data$image_id)))
  })

  test_that("calculate_ice_seals_totals works correctly", {
    # Get paths to the example files
    left_file <- system.file("extdata", "AKFSC", "ice_seals_2025_fl223_L_ir_detections_validated.csv", package = "Optics")
    center_file <- system.file("extdata", "AKFSC", "ice_seals_2025_fl223_C_ir_detections_validated.csv", package = "Optics")
    right_file <- system.file("extdata", "AKFSC", "ice_seals_2025_fl223_R_ir_detections_validated.csv", package = "Optics")

    # Calculate totals
    totals <- calculate_ice_seals_totals(
      left_file = left_file,
      center_file = center_file,
      right_file = right_file
    )

    # Check the output format
    expect_type(totals, "list")
    expect_named(totals, c("left", "center", "right"))

    # Check that the counts are numeric
    expect_type(totals$left, "integer")
    expect_type(totals$center, "integer")
    expect_type(totals$right, "integer")
    
    # Check the validation logic
    expect_lte(totals$center, totals$left)
    expect_lte(totals$center, totals$right)
  })

  test_that("select_ice_seals_candidates works correctly", {
    # Get paths to the example files
    left_file <- system.file("extdata", "AKFSC", "ice_seals_2025_fl223_L_ir_detections_validated.csv", package = "Optics")
    center_file <- system.file("extdata", "AKFSC", "ice_seals_2025_fl223_C_ir_detections_validated.csv", package = "Optics")
    right_file <- system.file("extdata", "AKFSC", "ice_seals_2025_fl223_R_ir_detections_validated.csv", package = "Optics")
    
    # Ingest the data
    ice_seals_detections <- ingest_ice_seals_csv(c(left_file, center_file, right_file))
    
    # Select candidates
    candidates <- select_ice_seals_candidates(ice_seals_detections)
    
    # Check that the output is a data.frame
    expect_s3_class(candidates, "data.frame")
    
    # Check that only 'animal' category is present
    expect_true(all(candidates$category_name == "animal"))
    
    # Check that score is >= 0.9
    expect_true(all(candidates$score >= 0.9))
  })
})
