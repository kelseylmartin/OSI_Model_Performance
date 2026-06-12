#' Align Model-derived Counts with Ground Truth Counts
#'
#' Performs a full join on model-derived and ground-truth count data frames
#' to create a single, aligned data frame for performance evaluation.
#'
#' This function is designed to work with aggregated data, such as the output
#' from `calculate_maxn()` or `calculate_frame_abundance()`. It aligns the two
#' datasets using a specified set of grouping columns (e.g., `video_id`,
#' `category_name`). The resulting data frame will contain columns for both
#' model and manual counts, with `NA` values replaced by 0 to represent
#' false positives and false negatives correctly.
#'
#' @param model_counts A data frame or tibble containing counts derived from
#'   the model's predictions.
#' @param truth_counts A data frame or tibble containing counts from the
#'   human-annotated ground truth.
#' @param by A character vector of column names to join the two data frames on.
#'   These should be the columns that uniquely identify an observation group
#'   (e.g., `c("video_id", "category_name")`).
#' @param model_col The unquoted name of the column in `model_counts` that
#'   contains the count/metric data. Defaults to `maxn`.
#' @param truth_col The unquoted name of the column in `truth_counts` that
#'   contains the count/metric data. Defaults to `maxn`.
#' @return A `tibble` containing the aligned data. It will include the `by`
#'   columns and two new columns, `model_count` and `truth_count`, with `NA`
#'   values replaced by 0.
#' @export
#' @importFrom dplyr full_join rename select mutate across everything
#' @importFrom rlang enquo as_name
#' @importFrom tidyselect all_of
#' @importFrom magrittr %>%
#' @examples
#' model_df <- dplyr::tibble(
#'   video_id = c("v1", "v1", "v2"),
#'   category_name = c("FishA", "FishB", "FishA"),
#'   model_maxn = c(10, 1, 5)
#' )
#'
#' truth_df <- dplyr::tibble(
#'   video_id = c("v1", "v1", "v3"),
#'   category_name = c("FishA", "FishC", "FishA"),
#'   truth_maxn = c(12, 2, 8)
#' )
#'
#' align_counts(
#'   model_counts = model_df,
#'   truth_counts = truth_df,
#'   by = c("video_id", "category_name"),
#'   model_col = model_maxn,
#'   truth_col = truth_maxn
#' )
align_counts <- function(model_counts, truth_counts, by, model_col = maxn, truth_col = maxn) {

  model_col_quo <- rlang::enquo(model_col)
  truth_col_quo <- rlang::enquo(truth_col)

  model_col_name <- rlang::as_name(model_col_quo)
  truth_col_name <- rlang::as_name(truth_col_quo)

  # Ensure count columns exist, even if the data frame is empty, to prevent rename errors.
  if (!model_col_name %in% names(model_counts)) {
    model_counts[[model_col_name]] <- numeric(0)
  }
  if (!truth_col_name %in% names(truth_counts)) {
    truth_counts[[truth_col_name]] <- numeric(0)
  }

  # --- FIX: Rename the columns BEFORE joining ---
  model_counts <- model_counts %>%
    dplyr::rename(model_count = !!model_col_quo)
    
  truth_counts <- truth_counts %>%
    dplyr::rename(truth_count = !!truth_col_quo)

  # --- Now perform the join ---
  aligned_df <- dplyr::full_join(model_counts, truth_counts, by = by) %>%
    # Use dplyr::across safely inside mutate()
    dplyr::mutate(dplyr::across(c("model_count", "truth_count"), ~ifelse(is.na(.), 0, .)))

  return(aligned_df)
}