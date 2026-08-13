test_that("pkgdown reference covers current exported workflow helpers", {
  pkgdown_config <- readLines(test_path("..", "..", "_pkgdown.yml"), warn = FALSE)
  pkgdown_text <- paste(pkgdown_config, collapse = "\n")

  for (topic in c(
    "calculate_legacy_metrics",
    "calculate_percent_metric",
    "ingest_ice_seals_csv",
    "calculate_ice_seals_totals",
    "select_ice_seals_candidates"
  )) {
    expect_match(pkgdown_text, topic, fixed = TRUE)
  }
})

test_that("homepage walkthrough highlights the current bundled workflow", {
  readme_rmd <- paste(readLines(test_path("..", "..", "README.Rmd"), warn = FALSE), collapse = "\n")
  readme_md <- paste(readLines(test_path("..", "..", "README.md"), warn = FALSE), collapse = "\n")

  for (text in c(
    "system.file(\"extdata/AKFSC\", package = \"Optics\")",
    "ingest_ice_seals_csv(",
    "calculate_ice_seals_totals(",
    "select_ice_seals_candidates(",
    "calculate_legacy_metrics()",
    "calculate_percent_metric()"
  )) {
    expect_match(readme_rmd, text, fixed = TRUE)
    expect_match(readme_md, text, fixed = TRUE)
  }
})
