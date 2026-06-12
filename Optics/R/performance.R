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
#' @importFrom rlang .data syms
#' @importFrom magrittr %>%
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

  # Determine the total number of unique groups to get accurate TN counts using robust injection
  all_groups <- dplyr::bind_rows(
    dplyr::distinct(model_detections, !!!rlang::syms(by)),
    dplyr::distinct(truth_detections, !!!rlang::syms(by))
  )
  total_comparisons <- nrow(dplyr::distinct(all_groups))

  # Loop over each threshold, calculate metrics, and collect results
  all_metrics <- lapply(thresholds, function(thresh) {
    
    # Restored to strict filtering
    model_dets_filtered <- model_detections %>%
      dplyr::filter(.data$score >= thresh)

    if (nrow(model_dets_filtered) == 0) {
      # When the model detects nothing:
      fn_count <- sum(truth_counts[[ncol(truth_counts)]] > 0)
      metrics <- dplyr::tibble(
        tp = 0, fp = 0, fn = fn_count,
        precision = NA_real_, 
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