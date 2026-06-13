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
#' @import dplyr
#' @importFrom dplyr group_by summarise mutate
#' @importFrom rlang .data syms
#' @importFrom magrittr %>%
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
      dplyr::group_by(!!!rlang::syms(group_vars))
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
      dplyr::mutate(
        tn = total_comparisons - (.data$tp + .data$fp + .data$fn),
        accuracy = (.data$tp + .data$tn) / total_comparisons,
        fpr = .data$fp / (.data$fp + .data$tn), # False Positive Rate
        fnr = .data$fn / (.data$fn + .data$tp),  # False Negative Rate
        false_positive_ratio = .data$fp / (.data$tp + .data$fn + .data$tn),
        false_negative_ratio = .data$fn / (.data$tp + .data$fp + .data$tn)
      ) %>%
      dplyr::mutate(
        # Matthews Correlation Coefficient (MCC)
        # Numerator
        mcc_num = (as.numeric(.data$tp) * as.numeric(.data$tn)) - (as.numeric(.data$fp) * as.numeric(.data$fn)),
        # Denominator
        mcc_den = sqrt( (as.numeric(.data$tp) + as.numeric(.data$fp)) *
                        (as.numeric(.data$tp) + as.numeric(.data$fn)) *
                        (as.numeric(.data$tn) + as.numeric(.data$fp)) *
                        (as.numeric(.data$tn) + as.numeric(.data$fn)) ),
        mcc = ifelse(mcc_den == 0, 0, mcc_num / mcc_den)
      )
  }

  return(metrics_df)
}

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
#' @import dplyr
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

#' Classify Detections as True/False Positives
#'
#' Compares raw model detections against a human-validated set to classify each
#' raw detection as either a True Positive (TP) or a False Positive (FP).
#'
#' This function is designed for workflows where human annotators review and
#' correct the output of a model. It assumes that any raw detection that also
#' exists in the validated set is a True Positive, and any raw detection that
#' does not exist in the validated set was removed by a human and is therefore
#' a False Positive.
#'
#' @param raw_detections A data frame of raw detections from the model.
#' @param validated_detections A data frame of human-validated detections.
#' @param detection_id The unquoted column name that serves as the unique
#'   identifier for each detection (e.g., `annotation_id`). This ID must be
#'   consistent between the raw and validated data frames.
#' @return The `raw_detections` data frame with a new `status` column, where
#'   each detection is labeled as either "TP" or "FP". This output is suitable
#'   for use with `plot_roc_curve()` and `plot_pr_curve()`.
#' @export
#' @importFrom dplyr mutate anti_join bind_rows
#' @import dplyr
#' @importFrom rlang enquo as_name .data
#' @examples
#' raw <- dplyr::tibble(
#'   detection_id = 1:4,
#'   score = c(0.9, 0.7, 0.6, 0.4)
#' )
#' # Human keeps detections 1 & 3, deletes 2 & 4.
#' validated <- dplyr::tibble(
#'   detection_id = c(1, 3)
#' )
#'
#' classify_detections(raw, validated, detection_id = detection_id)
#'
classify_detections <- function(raw_detections, validated_detections, detection_id) {
  id_quo <- rlang::enquo(detection_id)
  id_col_name <- rlang::as_name(id_quo)

  if (!id_col_name %in% names(raw_detections) || !id_col_name %in% names(validated_detections)) {
    stop(paste("The detection ID column", id_col_name, "must exist in both data frames."))
  }

  # Detections in raw but NOT in validated are False Positives
  fp <- dplyr::anti_join(raw_detections, validated_detections, by = id_col_name) %>%
    dplyr::mutate(status = "FP")

  # Detections in raw AND in validated are True Positives
  tp <- dplyr::semi_join(raw_detections, validated_detections, by = id_col_name) %>%
    dplyr::mutate(status = "TP")

  return(dplyr::bind_rows(tp, fp))
}

#' Calculate a Confusion Matrix from Aligned Count Data
#'
#' This function transforms long-format aligned count data (manual vs. model)
#' into a standard confusion matrix. It determines misclassifications by
#' identifying where the model count is positive for one species while the
#' manual count is positive for another within the same deployment.
#' @param aligned_df A dataframe containing aligned manual and model counts.
#'   Must contain columns for grouping, species, model counts, and truth counts.
#' @param group_vars A character vector of column names that define the unit of
#'   analysis (e.g., `"Deployment"`).
#' @param species_col The unquoted name of the column containing species names.
#'   Defaults to `Species`.
#' @param model_col The unquoted name of the column with model counts. Defaults
#'   to `model_count`.
#' @param truth_col The unquoted name of the column with truth counts. Defaults
#'   to `truth_count`.
#' @return A `tibble` in the format `(Truth, Prediction, n)`, representing the
#'   confusion matrix. Also includes an "FN (No Prediction)" row for cases
#'   where the manual count was positive but the model made no prediction for
#'   any species in that deployment.
#' @export
#' @importFrom dplyr group_by summarize filter mutate select left_join ungroup
#' @import dplyr
#' @importFrom rlang enquo .data
#' @examples
#' \dontrun{
#'   # Assuming 'groundtruth_master' is your aligned data from Part II
#'   # and you want to analyze a specific model run:
#'   model_data <- groundtruth_master %>%
#'     dplyr::filter(year == 2022, Version == "v2", Confidence == 0.5) %>%
#'     rename(model_count = VIAME_MaxN, truth_count = Manual)
#'
#'   confusion_data <- calculate_confusion_matrix(model_data, group_vars = "Deployment")
#'   print(confusion_data)
#' }
calculate_confusion_matrix <- function(aligned_df, group_vars, species_col = Species, model_col = model_count, truth_col = truth_count) {

  required_cols <- c("Deployment", "Species", "Manual", "VIAME_MaxN")
  if (!all(required_cols %in% names(aligned_df))) {
    stop("Input dataframe must contain columns: Deployment, Species, Manual, VIAME_MaxN")
  }

  model_predictions <- aligned_df %>%
    dplyr::filter({{ model_col }} > 0) %>%
    dplyr::select(!!!rlang::syms(group_vars), Prediction = {{ species_col }})

  manual_annotations <- aligned_df %>%
    dplyr::filter({{ truth_col }} > 0) %>%
    dplyr::select(!!!rlang::syms(group_vars), Truth = {{ species_col }})

  dplyr::left_join(manual_annotations, model_predictions, by = group_vars, relationship = "many-to-many") %>%
    dplyr::mutate(Prediction = ifelse(is.na(Prediction), "FN (No Prediction)", Prediction)) %>%
    dplyr::group_by(Truth, Prediction) %>%
    dplyr::summarize(n = dplyr::n(), .groups = 'drop')
}

#' Analyze Reviewer Effort by Comparing Raw and Validated Detections
#'
#' Compares raw model detections to a human-validated set to quantify the
#' effort required for correction. This is useful for understanding model
#' behavior, such as over-segmentation or misclassification.
#'
#' The function calculates:
#' - `n_raw`: The total number of raw model detections.
#' - `n_validated`: The number of detections remaining after validation.
#' - `n_deleted`: The number of raw detections deleted by the reviewer.
#' - `n_reclassified`: The number of detections where the species class was changed.
#' - `avg_raw_per_validated`: The average number of raw detections that correspond
#'   to a single final validated detection, indicating over-segmentation.
#'
#' @param raw_df A data frame of raw model detections (e.g., from a VIAME CSV).
#'   Must contain `TrackID` and `Species`.
#' @param validated_df A data frame of the same detections after human review.
#'   Must contain `TrackID` and `Species`.
#' @param group_vars A character vector of column names to group the summary by
#'   (e.g., `c("Deployment", "UniqFrame")`).
#' @return A `tibble` summarizing the reviewer effort metrics for each group.
#' @export
#' @importFrom dplyr inner_join group_by summarise n_distinct n left_join
#' @import dplyr
#' @importFrom rlang syms .data
#' @examples
#' raw_detections <- dplyr::tibble(
#'   Deployment = "D1", UniqFrame = 1, TrackID = 1:5,
#'   Species = c("seal", "seal", "rock", "seal", "glare")
#' )
#' # Reviewer merges tracks 1,2,4 into a single "seal" (track 2),
#' # deletes track 3 ("rock") and 5 ("glare").
#' validated_detections <- dplyr::tibble(
#'   Deployment = "D1", UniqFrame = 1, TrackID = c(1, 2, 4),
#'   Species = c("seal", "seal", "seal")
#' )
#'
#' analyze_reviewer_effort(raw_detections, validated_detections, group_vars = "Deployment")
#'
analyze_reviewer_effort <- function(raw_df, validated_df, group_vars = NULL) {

  required_cols <- c("TrackID", "Species")
  if (!all(required_cols %in% names(raw_df)) || !all(required_cols %in% names(validated_df))) {
    stop("Both data frames must contain 'TrackID' and 'Species' columns.")
  }

  # Ensure grouping variables exist in both dataframes
  if (!is.null(group_vars)) {
    if (!all(group_vars %in% names(raw_df)) || !all(group_vars %in% names(validated_df))) {
      stop("All group_vars must be column names in both raw_df and validated_df.")
    }
  }

  # Define the columns to join by and group by
  join_by_cols <- c(group_vars, "TrackID")
  group_by_syms <- rlang::syms(group_vars)

  # Calculate reclassifications by joining on TrackID and comparing Species
  reclassified_counts <- dplyr::inner_join(raw_df, validated_df, by = join_by_cols, suffix = c("_raw", "_val")) %>%
    dplyr::filter(.data$Species_raw != .data$Species_val) %>%
    dplyr::group_by(!!!group_by_syms) %>%
    dplyr::summarise(n_reclassified = dplyr::n_distinct(.data$TrackID), .groups = "drop")

  # Calculate total raw and validated counts per group
  raw_counts <- raw_df %>%
    dplyr::group_by(!!!group_by_syms) %>%
    dplyr::summarise(n_raw = dplyr::n_distinct(.data$TrackID), .groups = "drop") %>%
    dplyr::left_join(reclassified_counts, by = group_vars)

  validated_counts <- validated_df %>%
    dplyr::group_by(!!!group_by_syms) %>%
    dplyr::summarise(n_validated = dplyr::n_distinct(.data$TrackID), .groups = "drop")

  # Combine all metrics
  dplyr::full_join(raw_counts, validated_counts, by = group_vars) %>%
    dplyr::mutate(
      n_deleted = .data$n_raw - .data$n_validated,
      avg_raw_per_validated = .data$n_raw / .data$n_validated,
      # Replace NA/NaN/Inf with 0 for cleaner output
      dplyr::across(dplyr::everything(), ~ifelse(is.na(.) | is.nan(.) | is.infinite(.), 0, .))
    )
}

#' Generate a Report of Top Disagreements
#'
#' Identifies and ranks the items (e.g., deployments, images, species) with
#' the highest number of false positives (FPs) and false negatives (FNs).
#' This is useful for pinpointing specific problem areas for model improvement.
#'
#' @param aligned_df A data frame containing aligned model and truth counts,
#'   typically from `align_counts()`. Must contain `model_count` and `truth_count`.
#' @param group_vars A character vector of column names to group by to identify
#'   the sources of disagreement (e.g., `c("Deployment", "Species")`).
#' @param top_n The number of top disagreements to return. Defaults to 10.
#' @return A `tibble` ranked by the total number of disagreements, showing the
#'   total FPs, FNs, and overall disagreement count for each group.
#' @export
#' @importFrom dplyr group_by summarise mutate arrange desc slice_head
#' @import dplyr
#' @importFrom rlang syms .data
#' @examples
#' aligned_data <- dplyr::tibble(
#'   Deployment = c("D1", "D1", "D2", "D2", "D3"),
#'   Species = c("Seal", "Rock", "Seal", "Glare", "Seal"),
#'   model_count = c(1, 1, 0, 1, 1),
#'   truth_count = c(1, 0, 1, 0, 0)
#' )
#' # D1: 1 TP (Seal), 1 FP (Rock)
#' # D2: 1 FN (Seal), 1 FP (Glare)
#' # D3: 1 FP (Seal)
#'
#' # Find which Deployments have the most errors
#' get_disagreement_report(aligned_data, group_vars = "Deployment")
#'
get_disagreement_report <- function(aligned_df, group_vars, top_n = 10) {
  required_cols <- c("model_count", "truth_count")
  if (!all(required_cols %in% names(aligned_df))) {
    stop("Input data frame must contain 'model_count' and 'truth_count' columns.")
  }

  aligned_df %>%
    dplyr::group_by(!!!rlang::syms(group_vars)) %>%
    dplyr::summarise(
      false_positives = sum(.data$model_count > 0 & .data$truth_count == 0),
      false_negatives = sum(.data$model_count == 0 & .data$truth_count > 0),
      .groups = "drop"
    ) %>%
    dplyr::mutate(total_disagreement = .data$false_positives + .data$false_negatives) %>%
    dplyr::arrange(dplyr::desc(.data$total_disagreement)) %>%
    dplyr::slice_head(n = top_n)
}

#' Analyze and Model Drivers of Performance Disagreement
#'
#' This function fits a Generalized Linear Mixed-Effects Model (GLMM) to
#' analyze what factors influence the magnitude of error between model and
#' truth counts. It helps identify characteristics of images/videos (e.g., high
#' species richness, high abundance) that lead to larger prediction errors.
#'
#' The function performs the following steps:
#' 1.  Aggregates data by the specified `group_vars` (e.g., "Deployment").
#' 2.  Calculates predictor variables for each group:
#'     - `n_species_truth`: The number of unique species in the ground truth.
#'     - `total_individuals_truth`: The total number of individuals in the ground truth.
#' 3.  Calculates a `weighted_error` as the sum of absolute differences between
#'     model and truth counts within the group.
#' 4.  Fits a `glmer` model (from the `lme4` package) to predict `weighted_error`
#'     based on the calculated predictors, with `Species` as a random effect.
#'
#' @param aligned_df A data frame containing aligned model and truth counts.
#'   Must contain `model_count`, `truth_count`, `Species`, and the specified
#'   `group_vars`.
#' @param group_vars A character vector of column names that define the unit of
#'   analysis (e.g., `c("Deployment", "year")`).
#' @return An object of class `glmerMod`, which is the fitted model from `lme4`.
#'   The summary of this model can be inspected to see which factors are
#'   significant predictors of model error.
#' @export
#' @importFrom dplyr group_by summarise mutate left_join n_distinct
#' @import dplyr
#' @importFrom rlang syms .data
#' @importFrom stats as.formula Gamma
#' @importFrom lme4 glmer
#' @examples
#' \dontrun{
#' # Assuming 'aligned_master' is a data frame with model_count, truth_count,
#' # Deployment, and Species columns.
#'
#' # Fit a model to see what drives errors at the Deployment level
#' performance_model <- analyze_performance_drivers(
#'   aligned_df = aligned_master,
#'   group_vars = "Deployment"
#' )
#'
#' # View the results
#' summary(performance_model)
#' }
analyze_performance_drivers <- function(aligned_df, group_vars) {
  required_cols <- c("model_count", "truth_count", "Species", group_vars)
  if (!all(required_cols %in% names(aligned_df))) {
    stop("Input data frame is missing one or more required columns.")
  }

  # 1. Calculate predictor variables and the weighted error metric per group
  model_data <- aligned_df %>%
    dplyr::group_by(!!!rlang::syms(group_vars)) %>%
    dplyr::summarise(
      # Predictors: characteristics of the ground truth
      n_species_truth = dplyr::n_distinct(.data$Species[.data$truth_count > 0]),
      total_individuals_truth = sum(.data$truth_count, na.rm = TRUE),
      # Response: a weighted error metric
      weighted_error = sum(abs(.data$model_count - .data$truth_count), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Add a small constant to the error to avoid issues with log(0) in Gamma models
    dplyr::mutate(weighted_error = .data$weighted_error + 0.001)

  # 2. Fit the GLMM
  # We model the weighted_error as a function of the truth characteristics.
  # We include Species as a random effect to account for the fact that some
  # species are inherently harder to detect than others.
  # We include the grouping variable (e.g., Deployment) as a random effect to
  # account for non-independence of observations within the same image/video.
  # A Gamma distribution is suitable for continuous, positive, skewed data like error counts.
  
  # Programmatically build the formula string
  random_effects_str <- paste0("(1 | ", group_vars, ")", collapse = " + ")
  full_formula_str <- paste(
    "weighted_error ~ n_species_truth + total_individuals_truth + (1 | Species) +",
    random_effects_str
  )
  final_formula <- stats::as.formula(full_formula_str)
  
  model_fit <- lme4::glmer(
    final_formula,
    data = dplyr::left_join(aligned_df, model_data, by = group_vars),
    family = stats::Gamma(link = "log")
  )

  return(model_fit)
}