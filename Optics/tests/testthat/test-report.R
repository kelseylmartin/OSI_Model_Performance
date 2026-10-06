report_fixture <- function(label = "FishA", grouping_level = "frame") {
  data <- data.frame(
    video_id = rep(c("v1", "v2", "v3", "v4"), 2),
    frame_index = rep(1:4, 2), category_name = label,
    threshold = rep(c(0.5, 0.9), each = 4),
    model_count = rep(c(1, 0, 1, 0), 2),
    truth_count = rep(c(1, 0, 299, 399), 2)
  )
  if (grouping_level == "video") data$frame_index <- NULL
  OpticsPerformance(data, grouping_level = grouping_level)
}

report_workspace <- function() {
  path <- tempfile(pattern = "optics-report-", tmpdir = tempdir())
  dir.create(path, showWarnings = FALSE)
  withr::defer(unlink(path, recursive = TRUE), envir = parent.frame())
  path
}

test_that("report wrapper passes isolated rendering parameters", {
  #' @description The wrapper passes the installed template, class copy, flags, paths, and isolated environment.
  object <- calculate_precision(report_fixture())
  captured <- NULL
  local_mocked_bindings(
    .class_report_dependencies = function() invisible(NULL),
    .render_class_report = function(...) {
      captured <<- list(...)
      "rendered-report.html"
    },
    .package = "Optics"
  )
  directory <- report_workspace()
  result <- withVisible(generate_class_report(
    object, "FishA", output_file = "chosen.html", output_dir = directory,
    quiet = FALSE
  ))
  expect_false(result$visible)
  expect_identical(result$value, "rendered-report.html")
  expect_identical(captured$input,
                   system.file("rmd", "class_performance_report.Rmd", package = "Optics"))
  expect_identical(captured$output_file, "chosen.html")
  expect_identical(captured$output_dir, directory)
  expect_identical(captured$intermediates_dir, directory)
  expect_identical(captured$params$class_label, "FishA")
  expect_identical(captured$params$grouping_level, "frame")
  expect_false(captured$params$remove_large_schools)
  expect_false(captured$quiet)
  expect_identical(parent.env(captured$envir), baseenv())
  expect_length(ls(captured$envir), 0)
  expect_identical(captured$params$object@data, object@data)
  generate_class_report(object, "FishA", output_dir = directory,
                        remove_large_schools = TRUE)
  expect_identical(captured$output_file, "class-performance-report.html")
  expect_true(captured$params$remove_large_schools)
  expect_true(captured$quiet)
  expect_false(any(captured$params$object@data$truth_count %in% c(299, 399, 999)))
})

test_that("report wrapper validates object, grouping, category and options", {
  #' @description Invalid report inputs fail before rendering or dependencies are needed.
  object <- report_fixture()
  expect_error(generate_class_report(data.frame(), "FishA"), "OpticsPerformance")
  expect_error(generate_class_report(object, "absent"), "class_label")
  empty <- object
  empty@data <- empty@data[FALSE, , drop = FALSE]
  expect_error(generate_class_report(empty, "FishA"), "class_label")
  expect_error(generate_class_report(object, c("FishA", "FishB")), "class_label")
  expect_error(generate_class_report(object, "FishA", grouping_level = "video"), "realign")
  expect_error(generate_class_report(object, "FishA", remove_large_schools = NA), "TRUE or FALSE")
  expect_error(generate_class_report(object, "FishA", quiet = "yes"), "TRUE or FALSE")
  expect_error(generate_class_report(object, "FishA", output_dir = ""), "output_dir")
  invalid <- object
  invalid@data$model_count[1] <- -1
  expect_error(generate_class_report(invalid, "FishA"), "invalid")
})

test_that("school filtering recomputes only selected metrics without modifying input", {
  #' @description School codes are opt-in exclusions; only chosen metrics and class thresholds are retained.
  object <- report_fixture()
  object@data$truth_count[4] <- 999
  object <- calculate_precision(object, confidence_thresholds = 0.5)
  original <- object
  retained <- Optics:::.prepare_class_report(object, "FishA", FALSE)
  filtered <- Optics:::.prepare_class_report(object, "FishA", TRUE)
  expect_identical(object, original)
  expect_equal(retained@data$truth_count, c(1, 0, 299, 999))
  expect_equal(filtered@data$truth_count, c(1, 0))
  expect_identical(names(filtered@metrics), c("category_name", "threshold", "precision"))
  expect_equal(filtered@metrics$threshold, 0.5)
  expect_equal(filtered@metrics$precision, 1)
  expect_false(any(c("tp", "fp", "fn", "tn", "recall") %in% names(filtered@metrics)))
  for (metric in c("agree", "difference", "relaxed")) {
    selected <- get(paste0("calculate_", metric))(original)
    filtered <- Optics:::.prepare_class_report(selected, "FishA", TRUE)
    expect_setequal(names(filtered@metrics),
                    c("category_name", "threshold", "precision", metric))
    expect_true(all(filtered@metrics[[metric]] == if (metric == "difference") 0 else 1))
    expect_false(identical(filtered@metrics[[metric]], selected@metrics[[metric]]))
  }
})

test_that("school filtering retains selected thresholds when all observations are removed", {
  #' @description Empty retained thresholds remain represented with missing metric values.
  object <- report_fixture()
  object@data$truth_count[object@data$threshold == 0.9] <- 299
  object <- calculate_agree(object)
  filtered <- Optics:::.prepare_class_report(object, "FishA", TRUE)
  expect_setequal(filtered@metrics$threshold, c(0.5, 0.9))
  expect_true(is.na(filtered@metrics$agree[filtered@metrics$threshold == 0.9]))
  expect_equal(filtered@metrics$agree[filtered@metrics$threshold == 0.5], 1)
  object@data$truth_count[] <- 299
  filtered <- Optics:::.prepare_class_report(object, "FishA", TRUE)
  expect_equal(nrow(filtered@data), 0)
  expect_setequal(filtered@metrics$threshold, c(0.5, 0.9))
  expect_true(all(is.na(filtered@metrics$agree)))
})

test_that("reports select only the requested category", {
  #' @description Class-specific metrics and counts exclude other categories, without mutating the input.
  object <- report_fixture()
  other <- object@data
  other$category_name <- "FishB"
  object@data <- rbind(object@data, other)
  object <- calculate_agree(object)
  filtered <- Optics:::.prepare_class_report(object, "FishB", FALSE)
  expect_identical(unique(filtered@data$category_name), "FishB")
  expect_identical(unique(filtered@metrics$category_name), "FishB")
  expect_setequal(unique(object@data$category_name), c("FishA", "FishB"))
})

test_that("report filtering preserves optional metadata group keys", {
  #' @description Extra grouping columns are retained as keys and never treated as metrics.
  data <- report_fixture()@data
  data$year <- rep(c(2024, 2024, 2025, 2025), 2)
  data$Version <- "model-v1"
  object <- OpticsPerformance(
    data, group_vars = c("category_name", "threshold", "year", "Version")
  )
  object <- calculate_agree(object)
  filtered <- Optics:::.prepare_class_report(object, "FishA", TRUE)
  expect_setequal(names(filtered@metrics), c(object@group_vars, "agree"))
  expect_true(methods::validObject(filtered))
  expect_equal(nrow(filtered@metrics), 4)
  expect_true(all(filtered@metrics$agree[filtered@metrics$year == 2024] == 1))
  expect_true(all(is.na(filtered@metrics$agree[filtered@metrics$year == 2025])))
  empty <- Optics:::.prepare_class_report(OpticsPerformance(
    data, group_vars = object@group_vars
  ), "FishA", TRUE)
  expect_identical(names(empty@metrics), object@group_vars)
})

test_that("school recomputation preserves each metric's selected threshold cells", {
  #' @description Metrics calculated at different thresholds do not gain unrequested values when filtered.
  object <- report_fixture()
  object <- calculate_precision(object, confidence_thresholds = 0.5)
  object <- calculate_f1(object, confidence_thresholds = 0.9)
  filtered <- Optics:::.prepare_class_report(object, "FishA", TRUE)
  expect_equal(filtered@metrics$precision[filtered@metrics$threshold == 0.5], 1)
  expect_true(is.na(filtered@metrics$precision[filtered@metrics$threshold == 0.9]))
  expect_equal(filtered@metrics$f1[filtered@metrics$threshold == 0.9], 1)
  expect_true(is.na(filtered@metrics$f1[filtered@metrics$threshold == 0.5]))
  expect_setequal(names(filtered@metrics), c("category_name", "threshold", "precision", "f1"))
  data <- object@data
  data$year <- rep(c(2024, 2024, 2025, 2025), 2)
  grouped <- OpticsPerformance(data, group_vars = c("category_name", "threshold", "year"))
  grouped <- calculate_precision(grouped, confidence_thresholds = 0.5)
  grouped <- calculate_f1(grouped, confidence_thresholds = 0.9)
  filtered <- Optics:::.prepare_class_report(grouped, "FishA", TRUE)
  expect_true(all(is.na(filtered@metrics$precision[filtered@metrics$threshold == 0.9])))
  expect_true(all(is.na(filtered@metrics$f1[filtered@metrics$threshold == 0.5])))
  expect_true(all(is.na(filtered@metrics$precision[filtered@metrics$year == 2025])))
  expect_true(methods::validObject(filtered))
})

test_that("template gates all optional metric sections with evaluated chunks", {
  #' @description Optional headings are emitted by gated as-is chunks, never static markdown headings.
  path <- system.file("rmd", "class_performance_report.Rmd", package = "Optics")
  expect_true(nzchar(path))
  template <- paste(readLines(path), collapse = "\n")
  expect_match(template, "theme: readable", fixed = TRUE)
  expect_match(template, "toc_float: true", fixed = TRUE)
  expect_match(template, "eval=length(optimal_metrics) > 0", fixed = TRUE)
  expect_match(template, "eval=length(count_metrics) > 0", fixed = TRUE)
  expect_match(template, "eval=all(false_metrics %in% chosen)", fixed = TRUE)
  expect_match(template, "eval=length(chosen) > 0", fixed = TRUE)
  expect_false(any(grepl("^#{1,6} ", readLines(path))))
  expect_false(grepl("calculate_[a-z]", template))
})

test_that("installed reports render with chosen-only sections and escaped labels", {
  #' @description Real HTML output gates headings and tables, escapes labels, and supports both grouping levels.
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not_installed("ggplot2")
  skip_if_not(rmarkdown::pandoc_available(), "Pandoc is required")
  directory <- report_workspace()
  label <- '<script>alert("label")</script> [link](https://example.invalid)'
  object <- report_fixture(label)
  render_html <- function(x, filename, ...) {
    path <- generate_class_report(x, label, filename, directory, ...)
    expect_true(file.exists(path))
    html <- paste(readLines(path, warn = FALSE), collapse = "\n")
    # Bundled syntax-highlighter scripts contain unrelated metric-name strings.
    gsub("<script\\b[^>]*>[\\s\\S]*?</script>", "", html, perl = TRUE)
  }
  empty <- render_html(object, "empty.html")
  expect_match(empty, "No performance metrics have been calculated", fixed = TRUE)
  expect_false(grepl("Optimal Confidence|Count Distribution|False Classification Table", empty))
  expect_false(grepl('<script>alert("label")</script>', empty, fixed = TRUE))
  expect_match(empty, "&lt;script&gt;", fixed = TRUE)
  expect_false(grepl('href="https://example.invalid"', empty, fixed = TRUE))
  precision <- render_html(calculate_precision(object), "precision.html")
  expect_match(precision, "Optimal Confidence", fixed = TRUE)
  expect_false(grepl("Count Distribution|False Classification Table|False positive rate|Exact agreement", precision))
  agree <- render_html(calculate_agree(object), "agree.html", remove_large_schools = TRUE)
  expect_match(agree, "Count Distribution", fixed = TRUE)
  expect_match(agree, "filtering is enabled", fixed = TRUE)
  expect_false(grepl("False Classification Table|Precision|Recall", agree))
  false_object <- calculate_tn(calculate_fn(calculate_fp(calculate_tp(object))))
  false <- render_html(false_object, "false.html")
  expect_match(false, "False Classification Table", fixed = TRUE)
  expect_false(grepl("Count Distribution|Precision|Recall", false))
  video <- render_html(calculate_relaxed(report_fixture(label, "video")), "video.html")
  expect_match(video, "video", fixed = TRUE)
  expect_match(video, "Relaxed agreement", fixed = TRUE)
  schools <- object
  schools@data$truth_count[] <- 299
  schools <- render_html(calculate_agree(schools), "schools.html",
                          remove_large_schools = TRUE)
  expect_match(schools, "Count Distribution", fixed = TRUE)
  expect_match(schools, "No finite metric values", fixed = TRUE)
  grouped_data <- object@data
  grouped_data$year <- rep(c(2024, 2024, 2025, 2025), 2)
  grouped_data$Version <- "model-v1"
  grouped <- OpticsPerformance(
    grouped_data, group_vars = c("category_name", "threshold", "year", "Version")
  )
  grouped <- render_html(calculate_agree(grouped), "grouped.html",
                          remove_large_schools = TRUE)
  expect_match(grouped, "model-v1", fixed = TRUE)
  expect_match(grouped, "2024", fixed = TRUE)
  expect_match(grouped, "2025", fixed = TRUE)
  expect_false(grepl("Precision|Recall|False Classification Table", grouped))
})
