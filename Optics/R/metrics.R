#' Calculate MaxN (Maximum Number of Individuals)
#'
#' Calculates the maximum number of individuals of each species category
#' observed in any single frame within each video. This is a standard metric
#' for baited remote underwater video (BRUV) surveys.
#'
#' @param detections_df A standardized detections tibble, as produced by one of
#'   the `read_*` ingestion functions. Must contain `video_id`, `frame_index`,
#'   and `category_name`.
#' @param group_cols A character vector of additional column names from
#'   `detections_df` to group by, besides `video_id` and `category_name`.
#'   This is useful for calculating metrics across different models or
#'   confidence thresholds (e.g., `c("version", "confidence")`).
#' @return A `tibble` with columns for each grouping variable (including
#'   `video_id` and `category_name`) and a `maxn` column containing the
#'   calculated MaxN value.
#' @export
#' @importFrom dplyr group_by summarise n n_distinct ungroup
#' @importFrom rlang .data
#' @examples
#' # Create a sample standardized data frame
#' sample_df <- dplyr::tibble(
#'   video_id = c(rep("video1", 4), rep("video2", 3)),
#'   frame_index = c(1, 1, 2, 2, 1, 1, 1),
#'   category_name = c("FishA", "FishA", "FishA", "FishB", "FishA", "FishA", "FishA"),
#'   annotation_id = 1:7
#' )
#'
#' # Calculate MaxN for each video and species
#' calculate_maxn(sample_df)
#'
calculate_maxn <- function(detections_df, group_cols = NULL) {

  # --- 1. Input Validation ---
  required_cols <- c("video_id", "frame_index", "category_name")
  if (!all(required_cols %in% names(detections_df))) {
    stop("Input data frame must contain columns: ", paste(required_cols, collapse = ", "))
  }

  all_groups <- c("video_id", "category_name", group_cols)

  # Guard Clause: If the input is empty, return a correctly structured empty tibble.
  if (nrow(detections_df) == 0) {
    all_return_cols <- c(all_groups, "maxn")
    return(dplyr::tibble(!!!stats::setNames(lapply(all_return_cols, function(x) logical(0)), all_return_cols)))
  }

  # --- 2. Calculate MaxN using base R aggregate for robustness ---
  # First, count detections per frame. Using `video_id` to count rows.
  frame_counts <- stats::aggregate(
    x = list(n_in_frame = detections_df$video_id),
    by = detections_df[, c(all_groups, "frame_index"), drop = FALSE],
    FUN = length
  )

  # Then, find the max of those counts for each group
  maxn_df <- stats::aggregate(
    x = list(maxn = frame_counts$n_in_frame),
    by = frame_counts[, all_groups, drop = FALSE],
    FUN = max
  )

  return(dplyr::as_tibble(maxn_df))
}

#' Calculate Frame-by-Frame Abundance
#' 
#' Calculates the number of detections for each species category in every frame
#' where they appear. This provides a time-series of counts.
#' 
#' @param detections_df A standardized detections tibble, as produced by one of
#'   the `read_*` ingestion functions. Must contain `video_id`, `frame_index`,
#'   and `category_name`.
#' @param group_cols A character vector of additional column names from
#'   `detections_df` to group by.
#' @return A `tibble` with columns for each grouping variable (including
#'   `video_id`, `frame_index`, and `category_name`) and an `abundance` column
#'   containing the per-frame counts.
#' @export
#' @importFrom dplyr group_by summarise n across all_of
#' @examples
#' # Create a sample standardized data frame
#' sample_df <- dplyr::tibble(
#'   video_id = c(rep("video1", 3)),
#'   frame_index = c(1, 1, 2),
#'   category_name = c("FishA", "FishA", "FishB"),
#'   annotation_id = 1:3
#' ) 
#'
#' # Calculate frame-by-frame abundance
#' calculate_frame_abundance(sample_df)
#'
calculate_frame_abundance <- function(detections_df, group_cols = NULL) {

  # --- 1. Input Validation ---
  required_cols <- c("video_id", "frame_index", "category_name")
  if (!all(required_cols %in% names(detections_df))) {
    stop("Input data frame must contain columns: ", paste(required_cols, collapse = ", "))
  }

  all_groups <- c("video_id", "frame_index", "category_name", group_cols)

  # Guard Clause: If the input is empty, return a correctly structured empty tibble.
  if (nrow(detections_df) == 0) {
    all_return_cols <- c(all_groups, "abundance")
    return(dplyr::tibble(!!!stats::setNames(lapply(all_return_cols, function(x) logical(0)), all_return_cols)))
  }

  # Use aggregate to count rows for each group
  frame_abundance <- stats::aggregate(
    x = list(abundance = detections_df$video_id),
    by = detections_df[, all_groups, drop = FALSE],
    FUN = length
  )

  return(dplyr::as_tibble(frame_abundance))
}