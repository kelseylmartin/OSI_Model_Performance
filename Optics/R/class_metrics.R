#' Aligned Performance Data and Selected Metrics
#'
#' Holds standardized comparisons independently of their ingestion format.
#' Binary metrics describe presence/absence per comparison, not bounding-box
#' matches. Count agreement metrics compare the actual abundances.
#'
#' @slot data Aligned counts with `category_name`, `threshold`, `model_count`,
#'   and `truth_count`, plus frame or video identifiers.
#' @slot metrics Summaries containing only explicitly selected metrics.
#' @slot grouping_level Either `"frame"` (abundance) or `"video"` (MaxN).
#' @slot group_vars Summary grouping columns, including category and threshold.
#' @exportClass OpticsPerformance
#' @rdname OpticsPerformance
setClass("OpticsPerformance",
         slots = c(data = "data.frame", metrics = "data.frame",
                   grouping_level = "character", group_vars = "character"))

setValidity("OpticsPerformance", function(object) {
  required <- unique(c("category_name", "threshold", "model_count",
                       "truth_count", object@group_vars))
  if (!all(required %in% names(object@data))) {
    return("Aligned data must contain category_name, threshold, model_count, truth_count and group_vars.")
  }
  if (length(object@grouping_level) != 1L ||
      !object@grouping_level %in% c("frame", "video")) {
    return("grouping_level must be 'frame' or 'video'.")
  }
  if (!all(c("category_name", "threshold") %in% object@group_vars) ||
      anyDuplicated(object@group_vars)) {
    return("group_vars must uniquely include category_name and threshold.")
  }
  counts <- object@data[c("model_count", "truth_count")]
  if (any(!vapply(counts, is.numeric, logical(1))) ||
      any(!is.finite(as.matrix(counts))) || any(as.matrix(counts) < 0)) {
    return("Counts must be finite, nonnegative numbers.")
  }
  thresholds <- object@data$threshold
  if (!is.numeric(thresholds) || any(!is.finite(thresholds)) ||
      any(thresholds < 0 | thresholds > 1)) {
    return("threshold must contain finite confidence values between zero and one.")
  }
  if (ncol(object@metrics) > 0 &&
      !all(object@group_vars %in% names(object@metrics))) {
    return("metrics must contain all group_vars.")
  }
  TRUE
})

#' @param data A standardized aligned count data frame.
#' @param grouping_level Aggregation used to create the counts.
#' @param group_vars Summary grouping columns; must include `category_name`
#'   and `threshold`. Additional metadata columns may be included.
#' @return An `OpticsPerformance` S4 object with no calculated metrics.
#' @export
#' @rdname OpticsPerformance
OpticsPerformance <- function(data, grouping_level = c("frame", "video"),
                              group_vars = c("category_name", "threshold")) {
  new("OpticsPerformance", data = as.data.frame(data),
      metrics = data.frame(), grouping_level = match.arg(grouping_level),
      group_vars = group_vars)
}

.validate_confidence_thresholds <- function(thresholds) {
  if (!is.numeric(thresholds) || !length(thresholds) ||
      any(!is.finite(thresholds)) || any(thresholds < 0 | thresholds > 1)) {
    stop("Confidence thresholds must be finite numbers between zero and one.")
  }
  unique(thresholds)
}

.metric_value <- function(data, metric) {
  model <- data$model_count
  truth <- data$truth_count
  tp <- sum(model > 0 & truth > 0)
  fp <- sum(model > 0 & truth == 0)
  fn <- sum(model == 0 & truth > 0)
  tn <- sum(model == 0 & truth == 0)
  ratio <- function(numerator, denominator) {
    if (denominator == 0) NA_real_ else numerator / denominator
  }
  switch(metric,
         tp = tp, fp = fp, fn = fn, tn = tn,
         precision = if (tp + fp == 0) 0 else tp / (tp + fp),
         recall = ratio(tp, tp + fn),
         f1 = if (2 * tp + fp + fn == 0) 0 else 2 * tp / (2 * tp + fp + fn),
         fpr = ratio(fp, fp + tn), fnr = ratio(fn, fn + tp),
         accuracy = ratio(tp + tn, length(model)),
         false_positive_ratio = ratio(fp, tp + fn + tn),
         false_negative_ratio = ratio(fn, tp + fp + tn),
         total_actual_positives = tp + fn,
         total_actual_negatives = fp + tn,
         agree = mean(model == truth),
         difference = mean(truth - model),
         relaxed = mean(abs(truth - model) <= 1))
}

.append_class_metric <- function(object, metric, confidence_thresholds) {
  if (!is(object, "OpticsPerformance")) {
    stop("object must be an OpticsPerformance S4 object from align_counts().")
  }
  validObject(object)
  data <- object@data
  if (!is.null(confidence_thresholds)) {
    thresholds <- .validate_confidence_thresholds(confidence_thresholds)
    available <- unique(data$threshold)
    indices <- vapply(thresholds, function(threshold) {
      matches <- which(abs(available - threshold) <= sqrt(.Machine$double.eps))
      if (length(matches)) matches[1] else NA_integer_
    }, integer(1))
    if (anyNA(indices)) {
      stop("Requested confidence thresholds were not aligned; rerun align_counts().")
    }
    data <- data[data$threshold %in% available[indices], , drop = FALSE]
  }
  if (!nrow(data)) {
    result <- data[FALSE, object@group_vars, drop = FALSE]
    result[[metric]] <- numeric()
  } else {
    result <- data %>%
      dplyr::group_by(!!!rlang::syms(object@group_vars)) %>%
      dplyr::group_modify(~ dplyr::tibble(!!metric := .metric_value(.x, metric))) %>%
      dplyr::ungroup()
  }
  if (!ncol(object@metrics)) {
    object@metrics <- as.data.frame(result)
  } else {
    existing <- object@metrics
    column_order <- unique(c(names(existing), metric))
    if (metric %in% names(existing)) {
      previous <- existing[c(object@group_vars, metric)]
      result <- dplyr::bind_rows(
        result, dplyr::anti_join(previous, result, by = object@group_vars)
      )
      existing[[metric]] <- NULL
    }
    object@metrics <- as.data.frame(
      dplyr::full_join(existing, result, by = object@group_vars)
    )[column_order]
  }
  validObject(object)
  object
}

#' Calculate an Individually Selected Performance Metric
#'
#' Each function returns the S4 object with only its named metric appended to
#' `@metrics`; dependencies are not exposed as calculated metrics. Summaries
#' are grouped by `@group_vars`. Confidence filtering is performed during
#' alignment; these functions optionally select already aligned thresholds.
#' Undefined ratios are `NA`; precision and F1 are zero when no positives
#' contribute to their denominators, matching the existing binary calculator.
#' `difference` is mean truth minus model count; `agree` is the proportion of
#' exact count matches and `relaxed` the proportion within one individual.
#'
#' @param object An `OpticsPerformance` S4 object returned by `align_counts()`.
#' @param confidence_thresholds Optional numeric vector of aligned thresholds
#'   between zero and one. Omit to calculate at all aligned thresholds.
#' @return The modified `OpticsPerformance` object.
#' @name calculate_class_metrics
NULL

#' @rdname calculate_class_metrics
#' @export
calculate_tp <- function(object, confidence_thresholds = NULL) {
  .append_class_metric(object, "tp", confidence_thresholds)
}
#' @rdname calculate_class_metrics
#' @export
calculate_fp <- function(object, confidence_thresholds = NULL) {
  .append_class_metric(object, "fp", confidence_thresholds)
}
#' @rdname calculate_class_metrics
#' @export
calculate_fn <- function(object, confidence_thresholds = NULL) {
  .append_class_metric(object, "fn", confidence_thresholds)
}
#' @rdname calculate_class_metrics
#' @export
calculate_tn <- function(object, confidence_thresholds = NULL) {
  .append_class_metric(object, "tn", confidence_thresholds)
}
#' @rdname calculate_class_metrics
#' @export
calculate_precision <- function(object, confidence_thresholds = NULL) {
  .append_class_metric(object, "precision", confidence_thresholds)
}
#' @rdname calculate_class_metrics
#' @export
calculate_recall <- function(object, confidence_thresholds = NULL) {
  .append_class_metric(object, "recall", confidence_thresholds)
}
#' @rdname calculate_class_metrics
#' @export
calculate_f1 <- function(object, confidence_thresholds = NULL) {
  .append_class_metric(object, "f1", confidence_thresholds)
}
#' @rdname calculate_class_metrics
#' @export
calculate_fpr <- function(object, confidence_thresholds = NULL) {
  .append_class_metric(object, "fpr", confidence_thresholds)
}
#' @rdname calculate_class_metrics
#' @export
calculate_fnr <- function(object, confidence_thresholds = NULL) {
  .append_class_metric(object, "fnr", confidence_thresholds)
}
#' @rdname calculate_class_metrics
#' @export
calculate_accuracy <- function(object, confidence_thresholds = NULL) {
  .append_class_metric(object, "accuracy", confidence_thresholds)
}
#' @rdname calculate_class_metrics
#' @export
calculate_false_positive_ratio <- function(object, confidence_thresholds = NULL) {
  .append_class_metric(object, "false_positive_ratio", confidence_thresholds)
}
#' @rdname calculate_class_metrics
#' @export
calculate_false_negative_ratio <- function(object, confidence_thresholds = NULL) {
  .append_class_metric(object, "false_negative_ratio", confidence_thresholds)
}
#' @rdname calculate_class_metrics
#' @export
calculate_total_actual_positives <- function(object, confidence_thresholds = NULL) {
  .append_class_metric(object, "total_actual_positives", confidence_thresholds)
}
#' @rdname calculate_class_metrics
#' @export
calculate_total_actual_negatives <- function(object, confidence_thresholds = NULL) {
  .append_class_metric(object, "total_actual_negatives", confidence_thresholds)
}
#' @rdname calculate_class_metrics
#' @export
calculate_agree <- function(object, confidence_thresholds = NULL) {
  .append_class_metric(object, "agree", confidence_thresholds)
}
#' @rdname calculate_class_metrics
#' @export
calculate_difference <- function(object, confidence_thresholds = NULL) {
  .append_class_metric(object, "difference", confidence_thresholds)
}
#' @rdname calculate_class_metrics
#' @export
calculate_relaxed <- function(object, confidence_thresholds = NULL) {
  .append_class_metric(object, "relaxed", confidence_thresholds)
}
