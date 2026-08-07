example_report <- testthat::test_path("..", "..", "inst", "examples", "e2e_gfisher_script_markdown.Rmd")
rewrite_script <- testthat::test_path("..", "..", "inst", "examples", "e2e_gfisher_script_rewrite.R")
legacy_report <- testthat::test_path("..", "..", "..", "Optics Model Performance Report.Rmd")

extract_report_scaffold <- function(path) {
  report_lines <- readLines(path, warn = FALSE)
  scaffold <- character()
  in_chunk <- FALSE

  for (line in report_lines) {
    if (grepl("^```\\{", line)) {
      in_chunk <- TRUE
      scaffold <- c(scaffold, trimws(line))
      next
    }

    if (grepl("^```\\s*$", line)) {
      in_chunk <- FALSE
      scaffold <- c(scaffold, "```")
      next
    }

    if (!in_chunk && (grepl("^# ", line) || trimws(line) %in% c("***", "<br>"))) {
      scaffold <- c(scaffold, trimws(line))
    }
  }

  scaffold
}

test_that("GFisher markdown example is added without replacing the legacy report", {
  expect_true(file.exists(example_report))
  expect_true(file.exists(rewrite_script))
  expect_true(file.exists(legacy_report))
  expect_false(identical(normalizePath(example_report), normalizePath(legacy_report)))
})

test_that("GFisher markdown example keeps the report structure and package pipeline", {
  report_text <- paste(readLines(example_report, warn = FALSE), collapse = "\n")

  for (section in c(
    "# `r spec_pretty` - Introduction",
    "# Optimal Confidence Across Years",
    "# Manual and VIAME Count Distribution",
    "# Analysis by Year {.tabset}"
  )) {
    expect_match(report_text, section, fixed = TRUE)
  }

  for (call in c(
    "read_viame_csv(",
    "read_wide_maxn(",
    "calculate_maxn(",
    "align_counts(",
    "calculate_binary_metrics("
  )) {
    expect_match(report_text, call, fixed = TRUE)
  }

  expect_match(report_text, "deployment-level `OpticsDetections` objects", fixed = TRUE)
  expect_false(grepl("\\bmerge\\(", report_text))
  expect_false(grepl("\\bcast\\(", report_text))
  expect_false(grepl("reshape::melt", report_text, fixed = TRUE))
})

test_that("GFisher markdown example preserves the legacy report scaffold", {
  example_lines <- readLines(example_report, warn = FALSE)
  legacy_lines <- readLines(legacy_report, warn = FALSE)

  expect_identical(
    grep('^title:', example_lines, value = TRUE)[1],
    grep('^title:', legacy_lines, value = TRUE)[1]
  )

  expect_identical(
    extract_report_scaffold(example_report),
    extract_report_scaffold(legacy_report)
  )
})

test_that("GFisher example scripts bootstrap their required packages", {
  report_text <- paste(readLines(example_report, warn = FALSE), collapse = "\n")
  rewrite_text <- paste(readLines(rewrite_script, warn = FALSE), collapse = "\n")

  for (script_text in list(report_text, rewrite_text)) {
    expect_match(script_text, "ensure_example_packages <- function", fixed = TRUE)
    expect_match(script_text, "install.packages(", fixed = TRUE)
    expect_match(script_text, "requireNamespace", fixed = TRUE)
    expect_match(script_text, "required_packages <- c(", fixed = TRUE)
  }

  expect_match(report_text, "\"FSA\"", fixed = TRUE)
  expect_match(report_text, "library(FSA)", fixed = TRUE)
  expect_match(rewrite_text, "\"rmarkdown\"", fixed = TRUE)
})
