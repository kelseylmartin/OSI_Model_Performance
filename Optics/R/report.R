#' Generate a Class Performance HTML Report
#'
#' Render a parameterized report using only metrics already selected on an
#' [OpticsPerformance] object. The report does not select or calculate
#' additional metrics.
#'
#' @param object A valid `OpticsPerformance` object with aligned counts and
#'   optionally calculated metrics.
#' @param class_label One category name present in `object@data`.
#' @param output_file HTML filename. Defaults to `"class-performance-report.html"`.
#' @param output_dir Output directory, created recursively if necessary.
#' @param remove_large_schools Logical; default `FALSE` retains counts unchanged.
#'   If `TRUE`, exclude aligned observations whose `truth_count` is 299, 399,
#'   or 999 and recalculate only the previously selected metric columns.
#' @param grouping_level `"frame"` or `"video"`. Must equal the object's
#'   grouping level: changing aggregation requires constructing a new object.
#' @param quiet Logical passed to [rmarkdown::render()].
#'
#' @details
#' Align counts at the desired grouping level, construct an `OpticsPerformance`
#' object, and call individual `calculate_*()` functions for the metrics wanted
#' before reporting. Confidence thresholds are the already aligned thresholds,
#' not a new detection-score filtering operation. Reports select one class and
#' retain its selected thresholds and the object's existing `group_vars`
#' (including optional metadata groups). No new grouping variables are added.
#'
#' School codes are a dataset-specific convention, not automatically interpreted
#' by Optics. Opting in removes whole aligned observations, not individual model
#' detections. It never recodes counts or caps model counts. Since metrics are
#' aggregated, filtering recomputes only existing selected columns on a copy;
#' the original object is unchanged. Selected thresholds with no remaining
#' observations are retained with missing (`NA`) metric values. Each metric's
#' original nonmissing group/threshold selection is preserved; filtering never
#' fills originally missing or unselected metric cells.
#'
#' Introduction is always shown. Optimal confidence, count distribution,
#' false-classification tables, and analysis tabs appear only when their
#' required metrics exist. Count distribution requires at least one of `agree`,
#' `difference`, or `relaxed`; the false table requires `tp`, `fp`, `fn`, and `tn`.
#' Optimal confidence is reported separately for each applicable chosen metric
#' and existing metadata group, with the highest threshold used to break ties,
#' rather than inventing a
#' weighted optimum. User labels are escaped in HTML.
#'
#' Rendering requires rmarkdown, knitr, ggplot2, and an available Pandoc.
#' The installed template is evaluated in an isolated environment, without
#' relying on objects in the calling session.
#'
#' @return Invisibly, the path returned by [rmarkdown::render()].
#' @export
#' @examples
#' \dontrun{
#' # performance is an OpticsPerformance object with aligned frame counts.
#' performance <- calculate_precision(performance)
#' performance <- calculate_agree(performance)
#' generate_class_report(performance, "FishA", output_dir = "reports")
#' # Exclude dataset-specific school codes and recompute precision/agree only.
#' generate_class_report(performance, "FishA", remove_large_schools = TRUE)
#' }
generate_class_report <- function(object, class_label, output_file = NULL,
                                  output_dir = getwd(),
                                  remove_large_schools = FALSE,
                                  grouping_level = object@grouping_level,
                                  quiet = TRUE) {
  if (!methods::is(object, "OpticsPerformance")) {
    stop("object must be an OpticsPerformance S4 object.", call. = FALSE)
  }
  methods::validObject(object)
  if (!is.character(class_label) || length(class_label) != 1L ||
      is.na(class_label) || !nzchar(class_label) ||
      !class_label %in% object@data$category_name) {
    stop("class_label must name one category present in object@data.", call. = FALSE)
  }
  if (!is.character(grouping_level) || length(grouping_level) != 1L ||
      is.na(grouping_level) || !grouping_level %in% c("frame", "video") ||
      !identical(grouping_level, object@grouping_level)) {
    stop("grouping_level must match object@grouping_level; realign counts to change aggregation.",
         call. = FALSE)
  }
  for (flag in c("remove_large_schools", "quiet")) {
    value <- get(flag)
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      stop(flag, " must be TRUE or FALSE.", call. = FALSE)
    }
  }
  if (is.null(output_file)) output_file <- "class-performance-report.html"
  for (argument in c("output_file", "output_dir")) {
    value <- get(argument)
    if (!is.character(value) || length(value) != 1L ||
        is.na(value) || !nzchar(value)) {
      stop(argument, " must be a nonempty character string.", call. = FALSE)
    }
  }
  .class_report_dependencies()
  template <- system.file("rmd", "class_performance_report.Rmd", package = "Optics")
  if (!nzchar(template)) {
    stop("The installed Optics report template is missing.", call. = FALSE)
  }
  if (!dir.exists(output_dir) &&
      !dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)) {
    stop("Cannot create output_dir.", call. = FALSE)
  }
  report_object <- .prepare_class_report(object, class_label, remove_large_schools)
  invisible(.render_class_report(
    input = template, output_file = output_file, output_dir = output_dir,
    intermediates_dir = output_dir,
    params = list(object = report_object, class_label = class_label,
                  remove_large_schools = remove_large_schools,
                  grouping_level = grouping_level),
    envir = new.env(parent = baseenv()), quiet = quiet
  ))
}

.class_report_dependencies <- function() {
  packages <- c("rmarkdown", "knitr", "ggplot2")
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("Report rendering requires: ", paste(missing, collapse = ", "), ".", call. = FALSE)
  }
  if (!rmarkdown::pandoc_available()) {
    stop("Report rendering requires an available Pandoc installation.", call. = FALSE)
  }
}

.render_class_report <- function(...) rmarkdown::render(...)

.prepare_class_report <- function(object, class_label, remove_large_schools) {
  result <- object
  group_vars <- result@group_vars
  result@data <- result@data[result@data$category_name == class_label, , drop = FALSE]
  if (!ncol(result@metrics)) {
    result@metrics <- result@data[FALSE, group_vars, drop = FALSE]
  } else {
    result@metrics <- result@metrics[result@metrics$category_name == class_label, , drop = FALSE]
  }
  selected <- setdiff(names(result@metrics), group_vars)
  thresholds <- unique(result@metrics$threshold)
  if (length(selected)) {
    result@data <- result@data[result@data$threshold %in% thresholds, , drop = FALSE]
  }
  if (remove_large_schools) {
    original_metrics <- result@metrics
    keys <- result@metrics[, group_vars, drop = FALSE]
    result@data <- result@data[!result@data$truth_count %in% c(299, 399, 999), , drop = FALSE]
    result@metrics <- keys
    for (metric in selected) {
      metric_keys <- original_metrics[!is.na(original_metrics[[metric]]),
                                     group_vars, drop = FALSE]
      metric_object <- result
      metric_object@data <- as.data.frame(
        dplyr::semi_join(result@data, metric_keys, by = group_vars)
      )
      metric_object@metrics <- keys[FALSE, , drop = FALSE]
      retained_thresholds <- intersect(metric_keys$threshold,
                                       unique(metric_object@data$threshold))
      calculator <- get(paste0("calculate_", metric), envir = asNamespace("Optics"))
      recalculated <- calculator(
        metric_object, confidence_thresholds =
          if (length(retained_thresholds)) retained_thresholds else NULL
      )
      result@metrics <- merge(result@metrics,
                              recalculated@metrics[, c(group_vars, metric), drop = FALSE],
                              by = group_vars, all.x = TRUE,
                              sort = FALSE)
    }
  }
  result
}
