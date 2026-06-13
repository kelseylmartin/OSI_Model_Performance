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
#' @importFrom dplyr group_by summarise n
#' @importFrom rlang .data syms
#' @import dplyr
#' @importFrom magrittr %>%
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

  all_groups <- unique(c("video_id", "category_name", group_cols))

  # Guard Clause: If the input is empty, return a correctly structured empty tibble.
  if (nrow(detections_df) == 0) {
    return(dplyr::tibble(!!!stats::setNames(lapply(c(all_groups, "maxn"), function(x) logical(0)), c(all_groups, "maxn"))))
  }

  # --- 2. Calculate MaxN with a standard, robust dplyr workflow ---
  detections_df %>%
    # First, count detections per frame using !!!rlang::syms() for safe injection
    dplyr::group_by(!!!rlang::syms(unique(c(all_groups, "frame_index")))) %>%
    dplyr::summarise(n_in_frame = dplyr::n(), .groups = "drop") %>%
    # Then, find the max of those counts for each group
    dplyr::group_by(!!!rlang::syms(all_groups)) %>%
    # max(c(0, ...)) ensures a 0 is returned for empty sets, preventing column drop.
    dplyr::summarise(maxn = max(c(0, .data$n_in_frame)), .groups = "drop")
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
#' @importFrom dplyr group_by summarise n
#' @importFrom rlang syms
#' @import dplyr
#' @importFrom magrittr %>%
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

  all_groups <- unique(c("video_id", "frame_index", "category_name", group_cols))

  # Guard Clause
  if (nrow(detections_df) == 0) {
    return(dplyr::tibble(!!!stats::setNames(lapply(c(all_groups, "abundance"), function(x) logical(0)), c(all_groups, "abundance"))))
  }

  # A simple group-and-count operation using symbol injection
  detections_df %>%
    dplyr::group_by(!!!rlang::syms(all_groups)) %>%
    dplyr::summarise(abundance = dplyr::n(), .groups = "drop")
}

#' Calculate Density from Counts and Area
#'
#' Calculates a density metric (e.g., individuals per square meter) by dividing
#' a count by a specified survey area.
#'
#' This function can operate in two ways:
#' 1. If a single numeric `area` is provided, it is applied to all rows.
#' 2. If an `area_col` is provided, the function uses the area values from that
#'    specific column for each row.
#'
#' You must provide either `area` or `area_col`, but not both.
#'
#' @param counts_df A data frame containing counts and, optionally, area information.
#' @param count_col The unquoted name of the column in `counts_df` that contains
#'   the count data (e.g., `maxn`).
#' @param area A single numeric value representing the survey area.
#' @param area_col An optional unquoted column name in `counts_df` that contains
#'   area values on a per-row basis.
#' @return The input `counts_df` with an added `density` column.
#' @export
#' @importFrom dplyr mutate
#' @import dplyr
#' @importFrom rlang enquo as_name .data
#' @examples
#' count_data <- dplyr::tibble(
#'   site = c("A", "B"),
#'   maxn = c(10, 25),
#'   survey_area_m2 = c(5, 10)
#' )
#'
#' # Using a fixed area for all sites
#' calculate_density(count_data, count_col = maxn, area = 100)
#'
#' # Using a per-site area from a column
#' calculate_density(count_data, count_col = maxn, area_col = survey_area_m2)
#'
calculate_density <- function(counts_df, count_col, area = NULL, area_col = NULL) {
  count_col_quo <- rlang::enquo(count_col)
  area_col_quo <- rlang::enquo(area_col)

  if (!is.null(area) && !rlang::quo_is_null(area_col_quo)) {
    stop("Please provide 'area' or 'area_col', but not both.")
  }

  if (!is.null(area)) {
    return(dplyr::mutate(counts_df, density = !!count_col_quo / area))
  } else {
    return(dplyr::mutate(counts_df, density = !!count_col_quo / !!area_col_quo))
  }
}