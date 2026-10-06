test_that("pkgdown reference covers current exported workflow helpers", {
  pkgdown_config <- readLines(test_path("..", "..", "_pkgdown.yml"), warn = FALSE)
  pkgdown_text <- paste(pkgdown_config, collapse = "\n")

  for (topic in c(
    "OpticsPerformance",
    "calculate_agree",
    "calculate_difference",
    "calculate_relaxed",
    "calculate_tp",
    "calculate_fp",
    "calculate_fn",
    "calculate_tn",
    "calculate_precision",
    "calculate_recall",
    "calculate_f1",
    "calculate_fpr",
    "calculate_fnr",
    "calculate_accuracy",
    "calculate_false_positive_ratio",
    "calculate_false_negative_ratio",
    "calculate_total_actual_positives",
    "calculate_total_actual_negatives",
    "generate_class_report",
    "calculate_percent_metric",
    "ingest_ice_seals_csv",
    "calculate_ice_seals_totals",
    "select_ice_seals_candidates"
  )) {
    expect_match(pkgdown_text, topic, fixed = TRUE)
  }
  expect_false(grepl("calculate_legacy_metrics", pkgdown_text, fixed = TRUE))
})

test_that("homepage walkthrough highlights the current bundled workflow", {
  readme_rmd <- paste(readLines(test_path("..", "..", "README.Rmd"), warn = FALSE), collapse = "\n")
  readme_md <- paste(readLines(test_path("..", "..", "README.md"), warn = FALSE), collapse = "\n")

  for (text in c(
    "system.file(\"extdata/AKFSC\", package = \"Optics\")",
    "scrape_gcp_uris(",
    "ingest_ice_seals_csv(",
    "calculate_ice_seals_totals(",
    "select_ice_seals_candidates(",
    "read_viame_csv(",
    "read_kwcoco(",
    "OpticsPerformance(data, grouping_level = \"video\"",
    "grouping_level = \"frame\"",
    "confidence_thresholds = c(0.5, 0.9)",
    "calculate_precision(performance)",
    "calculate_f1(performance)",
    "generate_class_report(",
    "remove_large_schools = FALSE",
    "truth codes 299, 399, or 999",
    "You cannot regroup after alignment",
    "reports only chosen metrics",
    "calculate_percent_metric()"
  )) {
    expect_match(readme_rmd, text, fixed = TRUE)
    expect_match(readme_md, text, fixed = TRUE)
  }
  expect_false(grepl("calculate_legacy_metrics", readme_rmd, fixed = TRUE))
  expect_false(grepl("calculate_legacy_metrics", readme_md, fixed = TRUE))
})
