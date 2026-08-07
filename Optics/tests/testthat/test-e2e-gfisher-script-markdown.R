example_report <- testthat::test_path("..", "..", "inst", "examples", "e2e_gfisher_script_markdown.Rmd")
legacy_report <- testthat::test_path("..", "..", "..", "Optics Model Performance Report.Rmd")

test_that("GFisher markdown example is added without replacing the legacy report", {
  expect_true(file.exists(example_report))
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
