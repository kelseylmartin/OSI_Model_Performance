#' Calculate Binary Classification Metrics
#'
#' Calculates common binary classification metrics (precision, recall, F1-score)
#' from an aligned data frame of model and ground truth counts.
#'
#' This function treats any non-zero count as a "presence" (a positive case)
#' and any zero count as an "absence" (a negative case). It calculates the
#' total number of true positives (TP), false positives (FP), and false
#' negatives (FN) across the dataset, optionally grouping by specified columns.
#'
#' @param aligned_df A data frame or tibble containing aligned counts, typically
#'   the output of `align_counts()`. Must contain `model_count` and `truth_count`.
#' @param group_vars A character vector of column names to group by before
#'   calculating metrics. This allows for performance evaluation across
#'   different categories (e.g., `c("year", "Version", "Species")`). If `NULL`
#'   (the default), metrics are calculated over the entire data frame.
#' @param total_comparisons An optional integer representing the total number of
#'   possible comparisons. If provided, True Negatives (TN) will be calculated.
#'   This is required for generating a full confusion matrix.
#' @return A `tibble` containing the grouping variables along with the
#'   calculated metrics: `tp`, `fp`, `fn`, `precision`, `recall`, and `f1_score`.
#' @export
#' @importFrom dplyr group_by summarise across all_of
#' @importFrom rlang .data
#' @examples
#' aligned_data <- dplyr::tibble(
#'   video_id = c("v1", "v1", "v2", "v1", "v3"),
#'   category_name = c("FishA", "FishB", "FishA", "FishC", "FishA"),
#'   model_count = c(10, 1, 5, 0, 0),
#'   truth_count = c(12, 0, 0, 2, 8)
#' )
#'
#' # Calculate metrics over the whole dataset
#' calculate_binary_metrics(aligned_data)
#'
#' # Calculate metrics grouped by category
#' calculate_binary_metrics(aligned_data, group_vars = "category_name")
#'
calculate_binary_metrics <- function(aligned_df, group_vars = NULL, total_comparisons = NULL) {

  # --- 1. Input Validation ---
  required_cols <- c("model_count", "truth_count")
  if (!all(required_cols %in% names(aligned_df))) {
    stop("Input data frame must contain columns: ", paste(required_cols, collapse = ", "))
  }

  # --- 2. Group if specified ---
  if (!is.null(group_vars)) {
    aligned_df <- aligned_df %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars)))
  }

  # --- 3. Calculate Metrics ---
  metrics_df <- aligned_df %>%
    dplyr::summarise(
      tp = sum(.data$model_count > 0 & .data$truth_count > 0),
      fp = sum(.data$model_count > 0 & .data$truth_count == 0),
      fn = sum(.data$model_count == 0 & .data$truth_count > 0),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      precision = .data$tp / (.data$tp + .data$fp),
      recall = .data$tp / (.data$tp + .data$fn),
      f1_score = 2 * (.data$precision * .data$recall) / (.data$precision + .data$recall)
    ) 

  if (!is.null(total_comparisons)) {
    # Can only calculate TN if we know the total number of comparisons
    metrics_df <- metrics_df %>%
      dplyr::mutate(tn = total_comparisons - (.data$tp + .data$fp + .data$fn))
  }

  return(metrics_df)
}

#' Summarize Performance Metrics Across Confidence Thresholds
#'
#' Evaluates model performance over a range of confidence thresholds,
#' generating metrics like precision, recall, and F1-score for each threshold.
#'
#' @param model_detections A standardized detections tibble for the model,
#'   which must include a `score` column.
#' @param truth_detections A standardized detections tibble for the ground truth.
#' @param metric_function The function to use for aggregating counts (e.g.,
#'   `calculate_maxn` or `calculate_frame_abundance`). Defaults to `calculate_maxn`.
#' @param by A character vector of column names to join the model and truth counts on.
#' @param thresholds A numeric vector of confidence thresholds to evaluate (e.g.,
#'   `seq(0.1, 0.9, 0.1)`).
#' @return A `tibble` with each row representing a confidence threshold and its
#'   corresponding performance metrics (TP, FP, FN, precision, recall, etc.).
#' @export
#' @importFrom dplyr filter bind_rows distinct
#' @importFrom rlang .data
#' @examples
#' # Create sample model and truth detection data
#' model_dets <- dplyr::tibble(
#'   video_id = "v1",
#'   frame_index = c(1, 1, 2),
#'   category_name = c("FishA", "FishA", "FishB"),
#'   score = c(0.95, 0.85, 0.7)
#' )
#' truth_dets <- dplyr::tibble(
#'   video_id = "v1",
#'   frame_index = c(1, 3),
#'   category_name = c("FishA", "FishC")
#' )
#'
#' # Summarize performance using MaxN across several thresholds
#' summarize_performance_by_threshold(
#'   model_detections = model_dets,
#'   truth_detections = truth_dets,
#'   by = c("video_id", "category_name"),
#'   thresholds = c(0.5, 0.8, 0.9)
#' )
summarize_performance_by_threshold <- function(model_detections,
                                               truth_detections,
                                               by,
                                               metric_function = calculate_maxn,
                                               thresholds = seq(0.1, 0.9, by = 0.1)) {

  # Calculate truth counts once, as they don't change
  truth_counts <- metric_function(truth_detections)

  # Determine the total number of unique groups to get accurate TN counts
  all_groups <- dplyr::bind_rows(
    dplyr::distinct(model_detections, dplyr::across(dplyr::all_of(by))),
    dplyr::distinct(truth_detections, dplyr::across(dplyr::all_of(by)))
  )
  total_comparisons <- nrow(dplyr::distinct(all_groups))

  # Loop over each threshold, calculate metrics, and collect results
  all_metrics <- lapply(thresholds, function(thresh) {
    model_dets_filtered <- model_detections %>%
      dplyr::filter(.data$score >= thresh)

    # If filtering results in no model detections, we can shortcut the process.
    # This is the definitive fix for the "column `maxn` doesn't exist" error.
    if (nrow(model_dets_filtered) == 0) {
      # When the model detects nothing:
      # TP and FP are 0.
      # FN is the total number of actual presences in the truth data.
      fn_count <- sum(truth_counts[[ncol(truth_counts)]] > 0)
      metrics <- dplyr::tibble(
        tp = 0, fp = 0, fn = fn_count,
        precision = NA_real_, # Or 0, depending on desired convention for 0/0
        recall = 0,
        f1_score = NA_real_,
        threshold = thresh
      )
    } else {
      # Proceed with normal calculation if there are detections
      model_counts <- metric_function(model_dets_filtered)
      aligned <- align_counts(model_counts, truth_counts, by = by)
      metrics <- calculate_binary_metrics(aligned, total_comparisons = total_comparisons)
      metrics$threshold <- thresh
    }
    return(metrics)
  })

  return(dplyr::bind_rows(all_metrics))
}