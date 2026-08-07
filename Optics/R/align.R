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

  model_col_quo <- rlang::enquo(model_col)
  truth_col_quo <- rlang::enquo(truth_col)

  model_col_name <- rlang::as_name(model_col_quo)
  truth_col_name <- rlang::as_name(truth_col_quo)

  # Ensure count columns exist, even if the data frame is empty
  if (!model_col_name %in% names(model_counts)) {
    model_counts[[model_col_name]] <- numeric(0)
  }
  if (!truth_col_name %in% names(truth_counts)) {
    truth_counts[[truth_col_name]] <- numeric(0)
  }

  model_counts_renamed <- model_counts %>% dplyr::rename(model_count = !!model_col_quo)
  truth_counts_renamed <- truth_counts %>% dplyr::rename(truth_count = !!truth_col_quo)

  aligned_df <- dplyr::full_join(model_counts_renamed, truth_counts_renamed, by = by) %>%
    dplyr::mutate(dplyr::across(c("model_count", "truth_count"), ~ifelse(is.na(.), 0, .)))

  return(aligned_df)
})
