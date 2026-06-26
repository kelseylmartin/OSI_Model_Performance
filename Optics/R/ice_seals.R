#' @title Functions for Ice Seal Dataset Processing
#' @description A collection of functions specific to the ice seal dataset.
#' @name ice_seals
NULL

#' @describeIn ice_seals Ingest and combine multiple Ice Seal CSV files.
#'
#' This function serves as a wrapper around \code{\link{read_viame_csv}} to
#' ingest multiple VIAME-formatted CSV files from the Ice Seals dataset (left,
#' center, and right views) and combine them into a single
#' \code{OpticsDetections} object.
#'
#' @param file_paths A character vector of full paths to the CSV files to be
#'   ingested.
#' @return A single \code{OpticsDetections} object containing the combined and
#'   standardized data from all provided files.
#' @export
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
ingest_ice_seals_csv <- function(file_paths) {
  # --- 1. Input Validation ---
  if (!is.character(file_paths) || length(file_paths) == 0) {
    stop("`file_paths` must be a character vector of one or more file paths.")
  }
  
  if (!all(file.exists(file_paths))) {
    stop("One or more files specified in `file_paths` do not exist.")
  }

  # --- 2. Ingest and Combine ---
  all_detections <- purrr::map(file_paths, ~read_viame_csv(.x, id_schema = "image_path")@data)
  
  combined_data <- dplyr::bind_rows(all_detections)

  # --- 3. Create OpticsDetections Object ---
  OpticsDetections(
    data = combined_data,
    source_file = paste(basename(file_paths), collapse = ", "),
    ingest_format = "viame_csv_ice_seals"
  )
}

#' @describeIn ice_seals Calculate and validate total animal counts from Ice Seal ground truth files.
#'
#' This function reads the ground truth CSV files for the left, center, and
#' right camera views, counts the number of valid animal detections in each,
#' and returns the totals. It also issues a warning if the number of detections
#' in the center view is greater than in the left or right views.
#'
#' @param left_file Path to the left view ground truth CSV.
#' @param center_file Path to the center view ground truth CSV.
#' @param right_file Path to the right view ground truth CSV.
#' @param valid_classes A character vector of class names to be considered as
#'   valid animal detections. Defaults to \code{c("animal", "animal_duplicate", "animal_new")}.
#' @return A named list with the total counts for "left", "center", and "right" views.
#' @export
#' @importFrom readr read_csv
calculate_ice_seals_totals <- function(left_file,
                                       center_file,
                                       right_file,
                                       valid_classes = c("animal", "animal_duplicate", "animal_new")) {

  count_animals <- function(file_path) {
    if (!file.exists(file_path)) {
      warning("File not found: ", file_path)
      return(0)
    }
    df <- suppressWarnings(readr::read_csv(file_path, comment = "#", col_names = FALSE, show_col_types = FALSE))
    # The class is in the 10th column
    as.integer(sum(df[[10]] %in% valid_classes))
  }

  totals <- list(
    left = count_animals(left_file),
    center = count_animals(center_file),
    right = count_animals(right_file)
  )

  if (totals$center > totals$left) {
    warning(sprintf("Center view has more animals (%d) than left view (%d).", totals$center, totals$left))
  }
  if (totals$center > totals$right) {
    warning(sprintf("Center view has more animals (%d) than right view (%d).", totals$center, totals$right))
  }

  return(totals)
}


#' @describeIn ice_seals Select candidate detections for review from the Ice Seals dataset.
#'
#' This function filters detections from an \code{OpticsDetections} object to
#' identify "candidate" animals for a secondary review. Candidates are defined
#' as having a high detection score and belonging to a specific category (e.g., "animal").
#'
#' @param detections An \code{OpticsDetections} object containing data from the
#'   Ice Seals dataset.
#' @param score_threshold The minimum score for a detection to be considered a
#'   candidate. Defaults to 0.9.
#' @param candidate_class The category name for candidate detections.
#'   Defaults to "animal".
#' @return A `data.frame` (or `tibble`) containing only the candidate
#'   detections.
#' @export
#' @importFrom dplyr filter
select_ice_seals_candidates <- function(detections,
                                      score_threshold = 0.9,
                                      candidate_class = "animal") {

  if (!inherits(detections, "OpticsDetections")) {
    stop("Input must be an OpticsDetections object.")
  }

  candidate_data <- detections@data %>%
    dplyr::filter(
      .data$score >= score_threshold,
      .data$category_name == candidate_class
    )
  
  return(candidate_data)
}
