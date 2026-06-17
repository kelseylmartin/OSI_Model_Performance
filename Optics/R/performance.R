#' @include ingest.R
#' @include metrics.R
#' @include align.R
NULL

#' Calculate Binary Classification Metrics
#'
#' @param aligned_df An object containing aligned counts.
#' @param ... Additional arguments passed to methods.
#' @export
#' @rdname calculate_binary_metrics
#' @examples
#' # Example using Erin's ice seal data.
#' # 1. Create temporary VIAME CSV files for model and truth data.
#' model_csv_data <- c(
#'   "1,video1,10,100,100,200,200,1,0.95,"ringed_seal",1",
#'   "2,video1,15,150,150,250,250,1,0.90,"bearded_seal",1"
#' )
#' truth_csv_data <- c(
#'   "1,video1,10,100,100,200,200,1,1.0,"ringed_seal",1",
#'   "3,video1,20,300,300,400,400,1,1.0,"ringed_seal",1"
#' )
#' model_csv_path <- tempfile(fileext = ".csv")
#' truth_csv_path <- tempfile(fileext = ".csv")
#' writeLines(c("# h1", "# h2", model_csv_data), model_csv_path)
#' writeLines(c("# h1", "# h2", truth_csv_data), truth_csv_path)
#'
#' # 2. Ingest and align data.
#' model_detections <- read_viame_csv(model_csv_path)
#' truth_detections <- read_viame_csv(truth_csv_path)
#' aligned_df <- align_counts(calculate_maxn(model_detections), calculate_maxn(truth_detections), by = c("video_id", "category_name"))
#'
#' # 3. Calculate binary metrics.
#' binary_metrics <- calculate_binary_metrics(aligned_df)
#' print(binary_metrics)
#'
#' # Clean up.
#' unlink(model_csv_path)
#' unlink(truth_csv_path)
setGeneric("calculate_binary_metrics", function(aligned_df, ...) {
  standardGeneric("calculate_binary_metrics")
})

#' @param group_vars A character vector of column names to group by.
#' @param total_comparisons An optional integer for the total number of comparisons.
#'
#' @rdname calculate_binary_metrics
#' @export
#' @importFrom dplyr group_by summarise mutate
#' @importFrom rlang .data syms
setMethod("calculate_binary_metrics", "data.frame",
          function(aligned_df, group_vars = NULL, total_comparisons = NULL) {
            # ... (implementation is the same as the original function)
            required_cols <- c("model_count", "truth_count")
            if (!all(required_cols %in% names(aligned_df))) {
              stop("Input data frame must contain columns: ", paste(required_cols, collapse = ", "))
            }
            
            if (!is.null(group_vars)) {
              aligned_df <- aligned_df %>%
                dplyr::group_by(!!!rlang::syms(group_vars))
            }
            
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
              metrics_df <- metrics_df %>%
                dplyr::mutate(
                  tn = total_comparisons - (.data$tp + .data$fp + .data$fn),
                  accuracy = (.data$tp + .data$tn) / total_comparisons,
                  fpr = .data$fp / (.data$fp + .data$tn),
                  fnr = .data$fn / (.data$fn + .data$tp),
                  false_positive_ratio = .data$fp / (.data$tp + .data$fn + .data$tn),
                  false_negative_ratio = .data$fn / (.data$tp + .data$fp + .data$tn)
                ) %>%
                dplyr::mutate(
                  mcc_num = (as.numeric(.data$tp) * as.numeric(.data$tn)) - (as.numeric(.data$fp) * as.numeric(.data$fn)),
                  mcc_den = sqrt( (as.numeric(.data$tp) + as.numeric(.data$fp)) *
                                    (as.numeric(.data$tp) + as.numeric(.data$fn)) *
                                    (as.numeric(.data$tn) + as.numeric(.data$fp)) *
                                    (as.numeric(.data$tn) + as.numeric(.data$fn)) ),
                  mcc = ifelse(mcc_den == 0, 0, mcc_num / mcc_den)
                )
            }
            
            return(metrics_df)
          })


#' Summarize Performance Metrics Across Confidence Thresholds
#'
#' @param model_detections An object containing model detections.
#' @param truth_detections An object containing truth detections.
#' @param ... Additional arguments passed to methods.
#' @export
#' @rdname summarize_performance_by_threshold
#' @examples
#' # Example using Erin's ice seal data.
#' model_csv_data <- c("1,video1,10,100,100,200,200,1,0.95,"ringed_seal",1")
#' truth_csv_data <- c("1,video1,10,100,100,200,200,1,1.0,"ringed_seal",1")
#' model_csv_path <- tempfile(fileext = ".csv")
#' truth_csv_path <- tempfile(fileext = ".csv")
#' writeLines(c("# h1", "# h2", model_csv_data), model_csv_path)
#' writeLines(c("# h1", "# h2", truth_csv_data), truth_csv_path)
#'
#' model_detections <- read_viame_csv(model_csv_path)
#' truth_detections <- read_viame_csv(truth_csv_path)
#'
#' performance_summary <- summarize_performance_by_threshold(
#'   model_detections = model_detections,
#'   truth_detections = truth_detections,
#'   by = c("video_id", "category_name")
#' )
#' print(performance_summary)
#' 
#' unlink(model_csv_path)
#' unlink(truth_csv_path)
setGeneric("summarize_performance_by_threshold", function(model_detections, truth_detections, ...) {
  standardGeneric("summarize_performance_by_threshold")
})

#' @param by A character vector of column names to join on.
#' @param metric_function The function to use for aggregation (e.g., `calculate_maxn`).
#' @param thresholds A numeric vector of confidence thresholds.
#' @rdname summarize_performance_by_threshold
#' @export
#' @importFrom dplyr filter bind_rows distinct
setMethod("summarize_performance_by_threshold",
          signature(model_detections = "OpticsDetections", truth_detections = "OpticsDetections"),
          function(model_detections, truth_detections, by,
                   metric_function = calculate_maxn,
                   thresholds = seq(0.1, 0.9, by = 0.1)) {
            
            model_df <- model_detections@data
            truth_df <- truth_detections@data
            
            truth_counts <- metric_function(truth_df)
            
            all_groups <- dplyr::bind_rows(
              dplyr::distinct(model_df, !!!rlang::syms(by)),
              dplyr::distinct(truth_df, !!!rlang::syms(by))
            )
            total_comparisons <- nrow(dplyr::distinct(all_groups))
            
            all_metrics <- lapply(thresholds, function(thresh) {
              model_dets_filtered <- model_df %>%
                dplyr::filter(.data$score >= thresh)
              
              if (nrow(model_dets_filtered) == 0) {
                fn_count <- sum(truth_counts[[ncol(truth_counts)]] > 0)
                metrics <- dplyr::tibble(
                  tp = 0, fp = 0, fn = fn_count,
                  precision = NA_real_, 
                  recall = 0,
                  f1_score = NA_real_,
                  threshold = thresh
                )
              } else {
                model_counts <- metric_function(model_dets_filtered)
                aligned <- align_counts(model_counts, truth_counts, by = by)
                metrics <- calculate_binary_metrics(aligned, total_comparisons = total_comparisons)
                metrics$threshold <- thresh
              }
              return(metrics)
            })
            
            return(dplyr::bind_rows(all_metrics))
          })

#' Classify Detections as True/False Positives
#'
#' @param raw_detections An object of raw detections.
#' @param validated_detections An object of validated detections.
#' @param ... Additional arguments.
#' @export
#' @rdname classify_detections
#' @examples
#' # Example using Erin's ice seal data.
#' model_csv_data <- c("1,video1,10,100,100,200,200,1,0.95,"ringed_seal",1")
#' truth_csv_data <- c("2,video1,10,110,110,210,210,1,1.0,"ringed_seal",1")
#' model_csv_path <- tempfile(fileext = ".csv")
#' truth_csv_path <- tempfile(fileext = ".csv")
#' writeLines(c("# h1", "# h2", model_csv_data), model_csv_path)
#' writeLines(c("# h1", "# h2", truth_csv_data), truth_csv_path)
#'
#' raw_detections <- read_viame_csv(model_csv_path)
#' validated_detections <- read_viame_csv(truth_csv_path)
#'
#' classified_detections <- classify_detections(
#'   raw_detections = raw_detections@data,
#'   validated_detections = validated_detections@data,
#'   detection_id = annotation_id
#' )
#' print(classified_detections)
#' 
#' unlink(model_csv_path)
#' unlink(truth_csv_path)
setGeneric("classify_detections", function(raw_detections, validated_detections, ...) {
  standardGeneric("classify_detections")
})

#' @param detection_id The unquoted column name for the unique detection identifier.
#' @rdname classify_detections
#' @export
#' @importFrom dplyr mutate anti_join bind_rows semi_join
setMethod("classify_detections",
          signature(raw_detections = "data.frame", validated_detections = "data.frame"),
          function(raw_detections, validated_detections, detection_id) {
            id_quo <- rlang::enquo(detection_id)
            id_col_name <- rlang::as_name(id_quo)
            
            if (!id_col_name %in% names(raw_detections) || !id_col_name %in% names(validated_detections)) {
              stop(paste("The detection ID column", id_col_name, "must exist in both data frames."))
            }
            
            fp <- dplyr::anti_join(raw_detections, validated_detections, by = id_col_name) %>%
              dplyr::mutate(status = "FP")
            
            tp <- dplyr::semi_join(raw_detections, validated_detections, by = id_col_name) %>%
              dplyr::mutate(status = "TP")
            
            return(dplyr::bind_rows(tp, fp))
          })

#' The rest of the functions are utilities that operate on generic data.frames.
#' They are converted to S4 generics for consistency.

#' @rdname calculate_confusion_matrix
#' @export
#' @examples
#' # Example using Abi's AUV data
#' model_csv_data <- c("1,video1,10,100,100,200,200,1,0.95,"sea_star",1")
#' truth_csv_data <- c("1,video1,10,100,100,200,200,1,1.0,"sea_anemone",1")
#' model_csv_path <- tempfile(fileext = ".csv")
#' truth_csv_path <- tempfile(fileext = ".csv")
#' writeLines(c("# h1", "# h2", model_csv_data), model_csv_path)
#' writeLines(c("# h1", "# h2", truth_csv_data), truth_csv_path)
#' 
#' model_detections <- read_viame_csv(model_csv_path)
#' truth_detections <- read_viame_csv(truth_csv_path)
#' 
#' aligned_df <- align_counts(
#'  calculate_maxn(model_detections), 
#'  calculate_maxn(truth_detections),
#'  by = c("video_id", "category_name")
#' )
#' 
#' conf_matrix <- calculate_confusion_matrix(aligned_df, 
#' group_vars = c("video_id"), species_col = category_name)
#' print(conf_matrix)
#' 
#' unlink(model_csv_path)
#' unlink(truth_csv_path)
setGeneric("calculate_confusion_matrix", function(aligned_df, ...) standardGeneric("calculate_confusion_matrix"))
#' @rdname calculate_confusion_matrix
#' @export
setMethod("calculate_confusion_matrix", "data.frame", function(aligned_df, group_vars, species_col = Species, model_col = model_count, truth_col = truth_count) {
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
})

#' Analyze Reviewer Effort
#'
#' @rdname analyze_reviewer_effort
#' @export
#' @examples
#' # Example using Erin's ice seal data
#' raw_csv_data <- c("1,video1,10,100,100,200,200,1,0.95,"ringed_seal",1")
#' validated_csv_data <- c("1,video1,10,100,100,200,200,1,1.0,"harbor_seal",1")
#' raw_csv_path <- tempfile(fileext = ".csv")
#' validated_csv_path <- tempfile(fileext = ".csv")
#' writeLines(c("# h1", "# h2", raw_csv_data), raw_csv_path)
#' writeLines(c("# h1", "# h2", validated_csv_data), validated_csv_path)
#' 
#' raw_df <- read_viame_csv(raw_csv_path)@data
#' validated_df <- read_viame_csv(validated_csv_path)@data
#' 
#' effort_analysis <- analyze_reviewer_effort(raw_df, validated_df)
#' print(effort_analysis)
#' 
#' unlink(raw_csv_path)
#' unlink(validated_csv_path)
setGeneric("analyze_reviewer_effort", function(raw_df, validated_df, ...) standardGeneric("analyze_reviewer_effort"))
#' @rdname analyze_reviewer_effort
#' @export
setMethod("analyze_reviewer_effort", signature(raw_df = "data.frame", validated_df = "data.frame"), function(raw_df, validated_df, group_vars = NULL) {
  required_cols <- c("TrackID", "Species")
  if (!all(required_cols %in% names(raw_df)) || !all(required_cols %in% names(validated_df))) {
    stop("Both data frames must contain 'TrackID' and 'Species' columns.")
  }
  if (!is.null(group_vars)) {
    if (!all(group_vars %in% names(raw_df)) || !all(group_vars %in% names(validated_df))) {
      stop("All group_vars must be column names in both raw_df and validated_df.")
    }
  }
  join_by_cols <- c(group_vars, "TrackID")
  group_by_syms <- rlang::syms(group_vars)
  reclassified_counts <- dplyr::inner_join(raw_df, validated_df, by = join_by_cols, suffix = c("_raw", "_val")) %>%
    dplyr::filter(.data$Species_raw != .data$Species_val) %>%
    dplyr::group_by(!!!group_by_syms) %>%
    dplyr::summarise(n_reclassified = dplyr::n_distinct(.data$TrackID), .groups = "drop")
  raw_counts <- raw_df %>%
    dplyr::group_by(!!!group_by_syms) %>%
    dplyr::summarise(n_raw = dplyr::n_distinct(.data$TrackID), .groups = "drop") %>%
    dplyr::left_join(reclassified_counts, by = group_vars)
  validated_counts <- validated_df %>%
    dplyr::group_by(!!!group_by_syms) %>%
    dplyr::summarise(n_validated = dplyr::n_distinct(.data$TrackID), .groups = "drop")
  dplyr::full_join(raw_counts, validated_counts, by = group_vars) %>%
    dplyr::mutate(
      n_deleted = .data$n_raw - .data$n_validated,
      avg_raw_per_validated = .data$n_raw / .data$n_validated,
      dplyr::across(dplyr::everything(), ~ifelse(is.na(.) | is.nan(.) | is.infinite(.), 0, .))
    )
})

#' Get a Report of Disagreements
#'
#' @rdname get_disagreement_report
#' @export
#' @examples
#' # Example using Tom & Michael's coral survey data
#' aligned_df <- dplyr::tibble(
#'   video_id = "v1", category_name = "Acropora", model_count = 5, truth_count = 10
#' )
#' disagreement_report <- get_disagreement_report(aligned_df, 
#' group_vars = c("video_id", "category_name"))
#' print(disagreement_report)
setGeneric("get_disagreement_report", function(aligned_df, ...) standardGeneric("get_disagreement_report"))
#' @rdname get_disagreement_report
#' @export
setMethod("get_disagreement_report", "data.frame", function(aligned_df, group_vars, top_n = 10) {
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
})

#' Analyze Performance Drivers
#'
#' @rdname analyze_performance_drivers
#' @export
#' @examples
#' \dontrun{
#' # For Tom & Michael's coral survey
#' aligned_df <- dplyr::tibble(
#'  video_id = "v1", Species = "Acropora", model_count = 5, truth_count = 10,
#'  n_species_truth = 1, total_individuals_truth = 10
#' )
#' performance_drivers <- analyze_performance_drivers(aligned_df, 
#' group_vars = c("video_id", "category_name"))
#' }
setGeneric("analyze_performance_drivers", function(aligned_df, ...) standardGeneric("analyze_performance_drivers"))
#' @rdname analyze_performance_drivers
#' @export
setMethod("analyze_performance_drivers", "data.frame", function(aligned_df, group_vars) {
  required_cols <- c("model_count", "truth_count", "Species", group_vars)
  if (!all(required_cols %in% names(aligned_df))) {
    stop("Input data frame is missing one or more required columns.")
  }
  model_data <- aligned_df %>%
    dplyr::group_by(!!!rlang::syms(group_vars)) %>%
    dplyr::summarise(
      n_species_truth = dplyr::n_distinct(.data$Species[.data$truth_count > 0]),
      total_individuals_truth = sum(.data$truth_count, na.rm = TRUE),
      weighted_error = sum(abs(.data$model_count - .data$truth_count), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(weighted_error = .data$weighted_error + 0.001)
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
})
