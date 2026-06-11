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

  # --- 2. Calculate Frame-Level Counts ---
  # Count the number of detections for each species in each frame of each video.
  all_groups <- c("video_id", "category_name", group_cols)
  frame_counts <- detections_df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(all_groups, "frame_index")))) %>%
    dplyr::summarise(n_in_frame = dplyr::n(), .groups = "drop")

  # --- 3. Calculate MaxN ---
  # For each video/species group, find the maximum count from any single frame.
  maxn_df <- frame_counts %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(all_groups))) %>%
    dplyr::summarise(maxn = max(.data$n_in_frame, na.rm = TRUE), .groups = "drop")

  # Ensure that if the input was empty, the output is also empty but with correct columns
  if (nrow(maxn_df) == 0) {
    return(dplyr::tibble(!!!c(setNames(list(character()), c("video_id", "category_name", group_cols)), list(maxn = numeric()))))
  }

  return(maxn_df)
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

  frame_abundance <- detections_df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(all_groups))) %>%
    dplyr::summarise(abundance = dplyr::n(), .groups = "drop")

  return(frame_abundance)
}