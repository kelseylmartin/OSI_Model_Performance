gfisher_report_example <- function() {
  environment <- new.env(parent = globalenv())
  sys.source(test_path("..", "..", "inst", "examples", "gfisher_class_report.R"),
             envir = environment)
  environment$run_gfisher_class_report
}

test_that("README versions explain how to run and open the GFISHER report", {
  #' @description Both README versions document prerequisites and the installed report example.
  for (file in c("README.Rmd", "README.md")) {
    text <- paste(readLines(test_path("..", "..", file), warn = FALSE), collapse = "\n")
    for (instruction in c(
      'install.packages(c("rmarkdown", "knitr"))',
      "rmarkdown::pandoc_available()",
      'source(system.file("examples", "gfisher_class_report.R", package = "Optics"))',
      "run_gfisher_class_report(",
      "result$report_paths",
      "utils::browseURL(normalizePath(path))",
      "reports/lutjanus_campechanus-performance.html",
      "calculate all 17 modular metrics",
      "one deployment and three unambiguously matched species"
    )) {
      expect_match(text, instruction, fixed = TRUE)
    }
  }
})

test_that("GFISHER report example calculates counts and every selected metric", {
  #' @description Bundled deployment counts use the manual window and matched species before reporting.
  captured <- list()
  local_mocked_bindings(
    generate_class_report = function(object, ...) {
      call <- list(object = object, ...)
      captured[[call$class_label]] <<- call
      call$output_file
    },
    .package = "Optics"
  )
  run_example <- gfisher_report_example()
  expect_message(result <- run_example(output_dir = tempdir()), "Analysis report")
  expect_s4_class(result$performance, "OpticsPerformance")
  expect_true(methods::validObject(result$performance))
  expect_equal(result$frame_bounds, c(3000, 9004))
  classes <- sort(unique(result$performance@data$category_name))
  expect_identical(names(result$report_paths), classes)
  expect_equal(unname(result$report_paths), paste0(tolower(classes), "-performance.html"))
  expect_null(result$report_path)
  expect_identical(names(captured), classes)
  counts <- result$performance@data
  selected <- counts[counts$threshold == 0.5, ]
  selected <- selected[order(selected$category_name), ]
  expect_equal(selected$model_count, c(2, 8, 1))
  expect_equal(selected$truth_count, c(2, 9, 1))
  expect_equal(nrow(counts), 6)
  expect_identical(unique(counts$video_id), "2024NCO155")
  expect_false(anyDuplicated(counts[c("video_id", "category_name", "threshold")]) > 0)
  expected <- c("tp", "fp", "fn", "tn", "precision", "recall", "f1", "fpr", "fnr",
                "accuracy", "false_positive_ratio", "false_negative_ratio",
                "total_actual_positives", "total_actual_negatives",
                "agree", "difference", "relaxed")
  expect_setequal(setdiff(names(result$performance@metrics), result$performance@group_vars),
                  expected)
  expect_equal(nrow(result$performance@metrics), 6)
  for (label in classes) {
    expect_identical(captured[[label]]$object, result$performance)
    expect_identical(captured[[label]]$class_label, label)
    expect_identical(captured[[label]]$grouping_level, "video")
    expect_identical(captured[[label]]$output_file, paste0(tolower(label), "-performance.html"))
    expect_false(captured[[label]]$remove_large_schools)
  }
  captured <- list()
  expect_message(single <- run_example(class_label = "LUTJANUS_CAMPECHANUS",
                                      output_dir = tempdir(),
                                      remove_large_schools = TRUE), "Analysis report")
  expect_identical(names(captured), "LUTJANUS_CAMPECHANUS")
  expect_identical(single$report_path, "lutjanus_campechanus-performance.html")
  expect_identical(single$report_paths, c(LUTJANUS_CAMPECHANUS =
                                          "lutjanus_campechanus-performance.html"))
  expect_true(captured[[1]]$remove_large_schools)
  expect_error(run_example(class_label = "unknown"), "class_label")
  expect_error(run_example(confidence_thresholds = NA_real_), "confidence_thresholds")
})

test_that("GFISHER example produces a complete HTML analysis report", {
  #' @description The example executes the real report wrapper with bundled GFISHER counts.
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not(rmarkdown::pandoc_available(), "Pandoc is required")
  directory <- tempfile("gfisher-report-", tmpdir = tempdir())
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  expect_message(result <- gfisher_report_example()(output_dir = directory), "Analysis report")
  classes <- sort(unique(result$performance@data$category_name))
  expect_length(result$report_paths, 3)
  expect_identical(names(result$report_paths), classes)
  expect_equal(length(unique(result$report_paths)), 3)
  expect_true(all(file.exists(result$report_paths)))
  expect_equal(sort(list.files(directory, pattern = "[.]html$")),
               paste0(tolower(classes), "-performance.html"))
  for (label in classes) {
    html <- paste(readLines(result$report_paths[[label]], warn = FALSE), collapse = "\n")
    html <- gsub("&amp;#95;", "_", html, fixed = TRUE)
    html <- gsub("&#95;", "_", html, fixed = TRUE)
    expect_true(grepl(label, html, fixed = TRUE))
    for (other in setdiff(classes, label)) {
      expect_false(grepl(other, html, fixed = TRUE))
    }
    expect_match(html, "False Classification Table", fixed = TRUE)
    expect_match(html, "Count Distribution", fixed = TRUE)
    expect_match(html, "Precision", fixed = TRUE)
    expect_match(html, "F1", fixed = TRUE)
  }
})
