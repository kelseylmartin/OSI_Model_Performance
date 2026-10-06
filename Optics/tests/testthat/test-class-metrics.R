class_detection_fixture <- function(frame, score, category = "fish", video = "v1") {
  n <- length(frame)
  OpticsDetections(data.frame(
    video_id = rep(video, length.out = n),
    image_id = if (n) paste0("image", frame) else character(), frame_index = frame,
    annotation_id = seq_len(n), category_name = rep(category, length.out = n),
    bbox_x = rep(0, n), bbox_y = rep(0, n),
    bbox_width = rep(1, n), bbox_height = rep(1, n), score = score
  ), "fixture", "test")
}

test_that("S4 alignment aggregates mixed scores before calculating MaxN", {
  model <- class_detection_fixture(c(1, 1, 2), c(.9, .8, .4))
  truth <- class_detection_fixture(c(1, 1, 2), rep(1, 3))
  frame <- align_counts(model, truth, grouping_level = "frame",
                        confidence_thresholds = c(.5, .95))
  expect_s4_class(frame, "OpticsPerformance")
  expect_true(methods::validObject(frame))
  expect_equal(frame@data$model_count, c(2, 0, 0, 0))
  expect_equal(frame@data$truth_count, c(2, 1, 2, 1))
  expect_equal(ncol(frame@metrics), 0)

  video <- align_counts(model, truth, grouping_level = "video",
                        confidence_thresholds = c(.5, .95))
  expect_equal(video@data$model_count, c(2, 0))
  expect_equal(video@data$truth_count, c(2, 2))
  expect_equal(calculate_fn(frame)@metrics$fn, c(1, 2))
})

test_that("each modular metric appends only the selected result", {
  object <- OpticsPerformance(data.frame(
    category_name = "fish", threshold = .5,
    model_count = c(2, 1, 0, 0), truth_count = c(1, 0, 1, 0)
  ))
  expected <- c(tp = 1, fp = 1, fn = 1, tn = 1, precision = .5,
                recall = .5, f1 = .5, fpr = .5, fnr = .5, accuracy = .5,
                false_positive_ratio = 1/3, false_negative_ratio = 1/3,
                total_actual_positives = 2, total_actual_negatives = 2,
                agree = .25, difference = -.25, relaxed = 1)
  for (metric in names(expected)) {
    fun <- get(paste0("calculate_", metric))
    result <- fun(object)
    expect_s4_class(result, "OpticsPerformance")
    expect_equal(names(result@metrics), c("category_name", "threshold", metric))
    expect_equal(result@metrics[[metric]], unname(expected[[metric]]))
    expect_equal(ncol(object@metrics), 0)
    expect_error(fun(object@data), "OpticsPerformance")
  }
  result <- calculate_f1(calculate_precision(object))
  expect_equal(names(result@metrics), c("category_name", "threshold", "precision", "f1"))
  expect_equal(calculate_precision(result)@metrics, result@metrics)
})

test_that("modular metrics handle undefined ratios, empty data, and thresholds", {
  model <- class_detection_fixture(1, NA_real_)
  truth <- class_detection_fixture(1, 1)
  object <- align_counts(model, truth, confidence_thresholds = c(.5, 1))
  expect_equal(object@data$model_count, c(0, 0))
  expect_equal(calculate_precision(object)@metrics$precision, c(0, 0))
  expect_equal(calculate_f1(object)@metrics$f1, c(0, 0))
  selected <- calculate_fp(calculate_precision(object, 1), .5)
  expect_equal(selected@metrics$threshold, c(1, .5))
  expect_true(is.na(selected@metrics$fp[1]))
  expect_true(is.na(selected@metrics$precision[2]))
  expect_error(calculate_precision(object, .2), "not aligned")
  for (threshold in list(NA_real_, Inf, -.1, 1.1, numeric(), "0.5")) {
    expect_error(align_counts(model, truth, confidence_thresholds = threshold),
                 "Confidence thresholds")
  }
  empty <- class_detection_fixture(integer(), numeric())
  aligned <- align_counts(empty, empty)
  expect_equal(nrow(aligned@data), 0)
  expect_equal(nrow(calculate_precision(aligned)@metrics), 0)
  zero <- OpticsPerformance(data.frame(
    category_name = "fish", threshold = 0, model_count = 0, truth_count = 0
  ))
  expect_true(is.na(calculate_recall(zero)@metrics$recall))
  expect_equal(calculate_accuracy(zero)@metrics$accuracy, 1)
  changed <- calculate_recall(object)
  changed@data$truth_count <- 0
  expect_true(all(is.na(calculate_recall(changed)@metrics$recall)))
  rounded <- align_counts(model, truth, confidence_thresholds = seq(.1, .9, by = .1))
  expect_equal(nrow(calculate_precision(rounded, .3)@metrics), 1)
  expect_error(OpticsPerformance(data.frame(
    category_name = "fish", threshold = .5, model_count = -1, truth_count = 0
  )), "nonnegative")
})

test_that("S4 image grouping does not merge frames without video metadata", {
  model <- class_detection_fixture(c(1, 2), c(.9, .8))
  model@data$video_id <- NA_character_
  model@data$frame_index <- NA_integer_
  truth <- model
  object <- align_counts(model, truth, grouping_level = "frame")
  expect_equal(nrow(object@data), 2)
  expect_equal(object@data$model_count, c(1, 1))
  expect_error(align_counts(model, truth, grouping_level = "video"), "requires")
})

test_that("standalone image identities override repeated frame indices", {
  model <- class_detection_fixture(0, .9)
  model@data$video_id <- NA_character_
  model@data$image_id <- "a.jpg"
  truth <- model
  truth@data$image_id <- "b.jpg"
  object <- align_counts(model, truth, grouping_level = "frame")
  expect_equal(nrow(object@data), 2)
  expect_equal(calculate_tp(object)@metrics$tp, 0)
  expect_equal(calculate_fp(object)@metrics$fp, 1)
  expect_equal(calculate_fn(object)@metrics$fn, 1)
})

test_that("standalone KWCOCO images pass through strict S4 alignment", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path))
  jsonlite::write_json(list(
    images = list(list(id = 1, file_name = "a.jpg"), list(id = 2, file_name = "b.jpg")),
    annotations = list(
      list(id = 1, image_id = 1, category_id = 1, bbox = c(0, 0, 1, 1), score = .9),
      list(id = 2, image_id = 2, category_id = 1, bbox = c(0, 0, 1, 1), score = .8)
    ),
    categories = list(list(id = 1, name = "fish"))
  ), path, auto_unbox = TRUE)
  model <- read_kwcoco(path)
  expect_s4_class(model, "OpticsDetections")
  object <- calculate_precision(align_counts(model, model, grouping_level = "frame"))
  expect_equal(nrow(object@data), 2)
  expect_equal(object@metrics$precision, 1)
})

test_that("CSV and KWCOCO produce the same S4 metric workflow", {
  json_path <- tempfile(fileext = ".json")
  csv_path <- tempfile(fileext = ".csv")
  on.exit(unlink(c(json_path, csv_path)))
  jsonlite::write_json(list(
    videos = list(list(id = 1, name = "v1")),
    images = list(list(id = 1, video_id = 1, frame_index = 1, file_name = "a.jpg")),
    annotations = list(
      list(id = 1, image_id = 1, category_id = 1, bbox = c(0, 0, 1, 1), score = .9),
      list(id = 2, image_id = 1, category_id = 1, bbox = c(0, 0, 1, 1), score = .5)
    ),
    categories = list(list(id = 1, name = "fish"))
  ), json_path, auto_unbox = TRUE)
  writeLines(c("# header", "# header",
               "1,v1,1,0,0,1,1,0.9,1,fish,1",
               "2,v1,1,0,0,1,1,0.5,1,fish,1"), csv_path)
  json <- read_kwcoco(json_path)
  csv <- read_viame_csv(csv_path, video_id = "v1")
  from_json <- calculate_precision(align_counts(json, json, confidence_thresholds = .5))
  from_csv <- calculate_precision(align_counts(csv, csv, confidence_thresholds = .5))
  expect_equal(from_json@data, from_csv@data)
  expect_equal(from_json@metrics, from_csv@metrics)
  expect_equal(from_json@data$model_count, 2)
})
