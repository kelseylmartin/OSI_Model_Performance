#' @include ingest.R class_metrics.R
NULL

#' Align Model-derived Counts with Ground Truth Counts
#'
#' This S4 generic performs a full join on model-derived and ground-truth
#' count data frames to create a single, aligned data frame for performance
#' evaluation.
#'
#' @param model_counts An object containing counts derived from the model's
#'   predictions (e.g., a data.frame or tibble).
#' @param truth_counts An object containing counts from the human-annotated
#'   ground truth.
#' @param ... Additional arguments passed to methods.
#'
#' @return A `tibble` containing the aligned data.
#'
#' @export
#' @rdname align_counts
setGeneric("align_counts", function(model_counts, truth_counts, ...) {
  standardGeneric("align_counts")
})

#' @param by A character vector of column names to join the two data frames on.
#' @param model_col The unquoted name of the column in `model_counts` that
#'   contains the count/metric data. Defaults to `maxn`.
#' @param truth_col The unquoted name of the column in `truth_counts` that
#'   contains the count/metric data. Defaults to `maxn`.
#'
#' @rdname align_counts
#' @export
#' @importFrom dplyr full_join rename mutate across
#' @importFrom rlang enquo as_name
#' @importFrom magrittr %>%
#' @examples
#' # Example using Erin's ice seal data to align model and truth counts.
#'
#' # 1. Create temporary VIAME CSV files for model and truth data.
#' model_csv_data <- c(
#'   "1,video1,10,100,100,200,200,1,0.95,\"ringed_seal\",1",
#'   "2,video1,15,150,150,250,250,1,0.90,\"bearded_seal\",1"
#' )
#' truth_csv_data <- c(
#'   "1,video1,10,100,100,200,200,1,1.0,\"ringed_seal\",1",
#'   "3,video1,20,300,300,400,400,1,1.0,\"ringed_seal\",1"
#' )
#'
#' model_csv_path <- tempfile(fileext = ".csv")
#' truth_csv_path <- tempfile(fileext = ".csv")
#'
#' writeLines(c("# header 1", "# header 2", model_csv_data), model_csv_path)
#' writeLines(c("# header 1", "# header 2", truth_csv_data), truth_csv_path)
#'
#' # 2. Ingest the data.
#' model_detections <- read_viame_csv(model_csv_path)
#' truth_detections <- read_viame_csv(truth_csv_path)
#'
#' # 3. Calculate MaxN counts for both.
#' model_counts <- calculate_maxn(model_detections)
#' truth_counts <- calculate_maxn(truth_detections)
#'
#' # 4. Align the counts.
#' aligned_df <- align_counts(
#'   model_counts = model_counts,
#'   truth_counts = truth_counts,
#'   by = c("video_id", "category_name")
#' )
#' print(aligned_df)
#'
#' # Clean up the temporary files.
#' unlink(model_csv_path)
#' unlink(truth_csv_path)
setMethod("align_counts",
          signature(model_counts = "data.frame", truth_counts = "data.frame"),
          function(model_counts, truth_counts, by, model_col = maxn, truth_col = maxn) {

  if (!"score" %in% names(model_counts)) {
    model_counts$score <- if ("Confidence" %in% names(model_counts)) as.numeric(model_counts$Confidence) else NA_real_
  }
  if (!"score" %in% names(truth_counts)) {
    truth_counts$score <- if ("Confidence" %in% names(truth_counts)) as.numeric(truth_counts$Confidence) else NA_real_
  }
  stopifnot("score" %in% colnames(model_counts))
  stopifnot("score" %in% colnames(truth_counts))

  model_col_quo <- rlang::enquo(model_col)
  truth_col_quo <- rlang::enquo(truth_col)

  model_col_name <- rlang::as_name(model_col_quo)
  truth_col_name <- rlang::as_name(truth_col_quo)

  # Ensure count columns exist, even if the data frame is empty
  if (!model_col_name %in% names(model_counts)) {
    model_counts[[model_col_name]] <- rep(0, nrow(model_counts))
  }
  if (!truth_col_name %in% names(truth_counts)) {
    truth_counts[[truth_col_name]] <- rep(0, nrow(truth_counts))
  }

  model_counts_renamed <- model_counts %>% dplyr::rename(model_count = !!model_col_quo)
  truth_counts_renamed <- truth_counts %>% dplyr::rename(truth_count = !!truth_col_quo)

  aligned_df <- dplyr::full_join(model_counts_renamed, truth_counts_renamed, by = by) %>%
    dplyr::mutate(dplyr::across(c("model_count", "truth_count"), ~ifelse(is.na(.), 0, .)))

  if (all(c("score.x", "score.y") %in% names(aligned_df))) {
    aligned_df <- aligned_df %>%
      dplyr::mutate(score = dplyr::coalesce(.data$score.x, .data$score.y)) %>%
      dplyr::select(-dplyr::any_of(c("score.x", "score.y")))
  }

  if ("score" %in% names(aligned_df)) {
    aligned_df <- aligned_df %>%
      dplyr::select(dplyr::any_of(c(by, "score")), dplyr::everything())
  }

  return(aligned_df)
})

#' @param grouping_level For S4 inputs, `"frame"` counts individuals per image;
#'   `"video"` uses the maximum frame abundance within each video.
#' @param confidence_thresholds For S4 inputs, numeric thresholds in `[0, 1]`.
#'   Predictions at or above each threshold are retained. Missing scores are
#'   excluded; truth is never thresholded.
#'   Comparisons from the unfiltered data remain when all predictions are removed.
#' @details The S4 comparison universe is the union of groups observed in model
#'   and truth detections. Groups absent from both inputs cannot be inferred;
#'   use `OpticsPerformance()` with explicit zero counts to include known empty
#'   comparisons. Missing frame indices use image identifiers in frame mode;
#'   video mode requires frame indices to calculate MaxN.
#' @return With two `OpticsDetections` inputs, an `OpticsPerformance` S4 object.
#' @rdname align_counts
#' @export
setMethod("align_counts",
          signature(model_counts = "OpticsDetections", truth_counts = "OpticsDetections"),
          function(model_counts, truth_counts, grouping_level = c("frame", "video"),
                   confidence_thresholds = 0) {
  validObject(model_counts)
  validObject(truth_counts)
  grouping_level <- match.arg(grouping_level)
  thresholds <- .validate_confidence_thresholds(confidence_thresholds)
  keys <- c("video_id", if (grouping_level == "frame") "frame_index", "category_name")
  frame_keys <- c("video_id", "frame_index", "category_name")
  prepare <- function(data) {
    if (!nrow(data)) {
      data <- data.frame(video_id = character(), frame_index = integer(),
                         category_name = character(), score = numeric())
    }
    if (nrow(data) && anyNA(data$frame_index)) {
      if (grouping_level == "video") {
        stop("Video aggregation requires frame_index; use grouping_level = 'frame' for images.")
      }
      missing_frame <- is.na(data$frame_index)
      data$frame_index <- as.character(data$frame_index)
      data$frame_index[missing_frame] <- paste0("image:", data$image_id[missing_frame])
    }
    if (nrow(data) && anyNA(data$video_id)) {
      if (grouping_level == "video") stop("Video aggregation requires video_id.")
      data$video_id[is.na(data$video_id)] <- "__images__"
    }
    data
  }
  model <- prepare(model_counts@data)
  truth <- prepare(truth_counts@data)
  if (is.character(model$frame_index) || is.character(truth$frame_index)) {
    model$frame_index <- as.character(model$frame_index)
    truth$frame_index <- as.character(truth$frame_index)
  }
  universe <- dplyr::distinct(dplyr::bind_rows(model[keys], truth[keys]))
  aggregate_counts <- function(data, name) {
    frames <- data %>%
      dplyr::group_by(!!!rlang::syms(frame_keys)) %>%
      dplyr::summarise(count = dplyr::n(), .groups = "drop")
    if (grouping_level == "video") {
      frames <- frames %>%
        dplyr::group_by(!!!rlang::syms(keys)) %>%
        dplyr::summarise(count = max(c(0L, .data$count)), .groups = "drop")
    }
    names(frames)[names(frames) == "count"] <- name
    frames
  }
  truth_aggregated <- aggregate_counts(truth, "truth_count")
  score <- suppressWarnings(as.numeric(model$score))
  runs <- lapply(thresholds, function(threshold) {
    selected <- model[!is.na(score) & score >= threshold, , drop = FALSE]
    universe %>%
      dplyr::left_join(aggregate_counts(selected, "model_count"), by = keys) %>%
      dplyr::left_join(truth_aggregated, by = keys) %>%
      dplyr::mutate(model_count = tidyr::replace_na(.data$model_count, 0L),
                    truth_count = tidyr::replace_na(.data$truth_count, 0L),
                    threshold = threshold)
  })
  OpticsPerformance(dplyr::bind_rows(runs), grouping_level = grouping_level)
})
