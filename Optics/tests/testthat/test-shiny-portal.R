test_that("bundled Shiny portal examples are discoverable", {
  #' @description Test that the Shiny portal discovers the expected packaged example pipelines.
  examples <- Optics:::.discover_optics_portal_examples()

  expect_true(all(c(
    "AUV Frame Abundance",
    "Aerial Ice Seals",
    "Stationary Benthic MaxN"
  ) %in% examples$label))
})

test_that("bundled Shiny portal examples produce populated analysis outputs", {
  #' @description Test that each packaged example produces populated metrics, confusion data, and aligned counts for the default app view.
  example_keys <- c(
    "auv_frame_abundance",
    "aerial_ice_seals",
    "stationary_benthic_maxn"
  )

  for (example_key in example_keys) {
    example_data <- Optics:::.optics_portal_load_example(example_key)
    analysis <- Optics:::.optics_portal_analyze(
      model_detections = example_data$model_detections,
      truth_detections = example_data$truth_detections,
      count_metric = example_data$default_count_metric,
      threshold = 0.5
    )

    expect_s3_class(analysis$performance_summary, "data.frame")
    expect_gt(nrow(analysis$performance_summary), 0)
    expect_true(all(c("precision", "recall", "f1_score") %in% names(analysis$performance_summary)))

    expect_s3_class(analysis$scalpred_summary, "data.frame")
    expect_gt(nrow(analysis$scalpred_summary), 0)

    expect_s3_class(analysis$aligned_counts, "data.frame")
    expect_gt(nrow(analysis$aligned_counts), 0)

    expect_s3_class(analysis$multiclass_confusion, "data.frame")
    expect_gt(nrow(analysis$multiclass_confusion), 0)
  }
})

test_that("truth-count uploads are converted into OpticsDetections rows", {
  #' @description Test that uploaded truth counts are expanded into standardized S4 detections.
  truth_upload <- data.frame(
    image_id = c("img_1", "img_2"),
    class_label = c("cod", "haddock"),
    true_count = c(2, 1),
    stringsAsFactors = FALSE
  )

  truth_obj <- Optics:::.optics_portal_standardize_upload(
    truth_upload,
    role = "truth",
    source_file = "truth.csv"
  )

  expect_s4_class(truth_obj, "OpticsDetections")
  expect_equal(nrow(truth_obj@data), 3)
  expect_true(all(truth_obj@data$score == 1))
})

test_that("Shiny portal UI exposes bundled-example navigation only", {
  #' @description Test that the packaged Shiny UI no longer exposes custom upload mode and instead includes the figure-view sidebar selector.
  skip_if_not_installed("shiny")
  ui_html <- htmltools::renderTags(Optics:::optics_portal_ui())$html

  expect_match(ui_html, "Figure view")
  expect_match(ui_html, "main_view")
  expect_false(grepl("Upload My Own Data", ui_html, fixed = TRUE))
  expect_false(grepl("Model predictions CSV", ui_html, fixed = TRUE))
})

test_that("packaged Shiny portal files are present", {
  #' @description Test that the packaged Shiny portal directory and app entrypoint exist.
  app_dir <- Optics:::.optics_portal_app_dir()
  example_script <- testthat::test_path("..", "..", "inst", "examples", "run_optics_app.R")

  expect_true(nzchar(app_dir))
  expect_true(dir.exists(app_dir))
  expect_true(file.exists(file.path(app_dir, "app.R")))
  expect_true(file.exists(example_script))
  expect_match(
    paste(readLines(example_script, warn = FALSE), collapse = "\n"),
    "run_optics_app\\s*\\(",
    perl = TRUE
  )
})
