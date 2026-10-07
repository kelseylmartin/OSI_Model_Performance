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
    expect_s4_class(example_data$model_detections, "OpticsDetections")
    expect_s4_class(example_data$truth_detections, "OpticsDetections")
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
    expect_true(all(c("model_count", "truth_count", "score") %in% names(analysis$aligned_counts)))
    expect_true(all(analysis$aligned_counts$score == 0.5))
    expect_true(all(analysis$aligned_counts$model_count >= 0))
    expect_true(all(analysis$aligned_counts$truth_count >= 0))

    expect_s3_class(analysis$multiclass_confusion, "data.frame")
    expect_gt(nrow(analysis$multiclass_confusion), 0)
  }
})

portal_detection_fixture <- function(frame, score) {
  OpticsDetections(data.frame(
    video_id = "v1", image_id = paste0("img", frame), frame_index = frame,
    annotation_id = seq_along(frame), category_name = "fish",
    bbox_x = 0, bbox_y = 0, bbox_width = 1, bbox_height = 1, score = score
  ), "fixture.csv", "test")
}

test_that("portal alignment retains truth when predictions are filtered out", {
  #' @description Test that selected-threshold counts exclude missing scores and preserve truth-only comparisons.
  model <- portal_detection_fixture(c(1, 2), c(0.9, NA_real_))
  truth <- portal_detection_fixture(c(1, 1), c(1, 0.7))

  for (count_metric in c("Frame Abundance", "MaxN")) {
    for (threshold in c(0.5, 1)) {
      expect_warning(
        analysis <- Optics:::.optics_portal_analyze(model, truth, count_metric, threshold),
        "NAs introduced while coercing"
      )
      expect_equal(analysis$aligned_counts$model_count, if (threshold == 1) 0 else 1)
      expect_equal(analysis$aligned_counts$truth_count, 2)
      expect_equal(analysis$aligned_counts$score, threshold)
      expect_false(anyNA(analysis$aligned_counts[c("model_count", "truth_count", "score")]))
      expect_gt(nrow(analysis$multiclass_confusion), 0)
    }
  }
  expect_equal(model@data$score, c(0.9, NA_real_))
  expect_equal(truth@data$score, c(1, 0.7))
})

test_that("portal class MaxN counts aggregate scores and retain truth-only videos", {
  #' @description Test class-specific server counts with mixed scores and no surviving predictions.
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  skip_if_not_installed("DT")
  dataset <- list(
    model_detections = portal_detection_fixture(c(1, 2), c(0.9, 0.8)),
    truth_detections = portal_detection_fixture(c(1, 1), c(1, 0.7)),
    allowed_count_metrics = "MaxN", default_count_metric = "MaxN"
  )
  local_mocked_bindings(
    .optics_portal_load_example = function(example_key) dataset,
    .package = "Optics"
  )
  shiny::testServer(Optics:::optics_portal_server, {
    session$setInputs(
      `data-example_key` = "fixture", count_metric = "MaxN",
      confidence_threshold = 0.5, class_selection = "fish"
    )
    results <- class_specific_results()
    counts <- results$model_abundance_per_video(0.5)
    expect_equal(counts$model_count, 1)
    expect_equal(counts$truth_count, 2)
    expect_equal(results$table_data$`Model Abundance`[1], 1)
    expect_equal(results$table_data$`Groundtruth Abundance`[1], 2)

    counts <- results$model_abundance_per_video(1)
    expect_equal(counts$model_count, 0)
    expect_equal(counts$truth_count, 2)
    expect_equal(counts$video_id, "v1")
    expect_equal(counts$category_name, "fish")
  })
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
