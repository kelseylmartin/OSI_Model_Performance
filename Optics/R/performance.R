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