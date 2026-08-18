test_that("bundled Shiny portal examples are discoverable", {
  #' @description Test that the Shiny portal discovers the expected packaged example pipelines.
  examples <- Optics:::.discover_optics_portal_examples()

  expect_true(all(c(
    "AUV Frame Abundance",
    "Aerial Ice Seals",
    "Stationary Benthic MaxN"
  ) %in% examples$label))
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
