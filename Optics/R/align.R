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
