#' @include theme.R
NULL

#' Plot a Scatterplot of Model vs. Truth Counts
#' @param aligned_df A data frame containing aligned counts.
#' @param ... Additional arguments.
#' @export
#' @rdname plot_counts_scatterplot
#' @examples
#' \dontrun{
#' erin_csv <- system.file("extdata", 
#' "ice_seals_2025_fl223_C_rgb_irDetectionsTransposed_processed.csv", package = "Optics")
#' model_detections <- read_viame_csv(erin_csv)
#' 
#' erin_truth_csv <- system.file("extdata", 
#' "ice_seals_2025_fl223_C_ir_detections_validated.csv", package = "Optics")
#' truth_detections <- read_viame_csv(erin_truth_csv)
#' 
#' model_counts <- calculate_maxn(model_detections)
#' truth_counts <- calculate_maxn(truth_detections)
#' 
#' aligned_df <- align_counts(
#'  model_counts, 
#'  truth_counts,
#'  by = c("video_id", "category_name"),
#'  model_col = maxn,
#'  truth_col = maxn
#'  )
#' 
#' plot_counts_scatterplot(aligned_df)
#' }
setGeneric("plot_counts_scatterplot", function(aligned_df, ...) standardGeneric("plot_counts_scatterplot"))

#' @param title An optional title for the plot.
#' @param model_col An optional unquoted column name for faceting by model.
#' @rdname plot_counts_scatterplot
#' @export
setMethod("plot_counts_scatterplot", "data.frame",
          function(aligned_df, title = "Model vs. Truth Counts", model_col = NULL) {
            # ... implementation from original function ...
            if (!all(c("model_count", "truth_count") %in% names(aligned_df))) {
              stop("Input data frame must contain 'model_count' and 'truth_count' columns.")
            }
            model_col_quo <- rlang::enquo(model_col)
            p <- ggplot2::ggplot(aligned_df, ggplot2::aes(x = .data$truth_count, y = .data$model_count)) +
              ggplot2::geom_point(alpha = 0.6, shape = 16) +
              ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
              ggpubr::stat_regline_equation(label.y.npc = 0.9, label.x.npc = 0.05, size = 4) +
              ggpubr::stat_regline_equation(label.y.npc = 0.8, label.x.npc = 0.05, aes(label = after_stat(rr.label)), size = 4) +
              ggplot2::labs(title = title, x = "Ground Truth Count", y = "Model Predicted Count") +
              theme_optics(base_size = 14) +
              ggplot2::coord_equal()
            if (!rlang::quo_is_null(model_col_quo)) {
              p <- p + ggplot2::facet_wrap(rlang::quo_get_expr(model_col_quo))
            }
            return(p)
          })

#' Plot a Precision-Recall (PR) Curve
#' @param detection_df A data frame of classified detections.
#' @param ... Additional arguments.
#' @export
#' @rdname plot_pr_curve
#' @examples
#' \dontrun{
#' # For Erin's ice seal survey
#' erin_csv <- system.file("extdata", 
#' "ice_seals_2025_fl223_C_rgb_irDetectionsTransposed_processed.csv", package = "Optics")
#' raw_detections <- read_viame_csv(erin_csv)
#' 
#' erin_truth_csv <- system.file("extdata", 
#' "ice_seals_2025_fl223_C_ir_detections_validated.csv", package = "Optics")
#' validated_detections <- read_viame_csv(erin_truth_csv)
#' 
#' classified_detections <- classify_detections(
#' raw_detections = raw_detections@data,
#' validated_detections = validated_detections@data,
#' detection_id = annotation_id
#' )
#' 
#' plot_pr_curve(classified_detections)
#' }
setGeneric("plot_pr_curve", function(detection_df, ...) standardGeneric("plot_pr_curve"))

#' @rdname plot_pr_curve
#' @export
setMethod("plot_pr_curve", "data.frame",
          function(detection_df, model_col = NULL, title = "Precision-Recall Curve") {
            # ... implementation from original function ...
            if (!all(c("score", "status") %in% names(detection_df))) {
              stop("Input data frame must contain 'score' and 'status' columns.")
            }
            model_col_quo <- rlang::enquo(model_col)
            if (rlang::quo_is_null(model_col_quo)) {
              model_col_name <- "model"
              detection_df[[model_col_name]] <- "Model"
              model_col_quo <- rlang::sym(model_col_name)
            }
            all_curves_data <- detection_df %>%
              dplyr::group_by(!!model_col_quo) %>%
              dplyr::do({
                df_group <- .
                tp_scores <- df_group %>% dplyr::filter(.data$status == "TP") %>% dplyr::pull(.data$score)
                fp_scores <- df_group %>% dplyr::filter(.data$status == "FP") %>% dplyr::pull(.data$score)
                if (length(tp_scores) == 0 && length(fp_scores) == 0) return(NULL)
                pr_obj <- PRROC::pr.curve(scores.class0 = fp_scores, scores.class1 = tp_scores, curve = TRUE)
                curve_df <- as.data.frame(pr_obj$curve)
                names(curve_df) <- c("recall", "precision", "threshold")
                curve_df$auc <- pr_obj$auc.integral
                curve_df
              }) %>%
              dplyr::ungroup() %>%
              dplyr::mutate(legend_label = paste0(!!model_col_quo, " (AUC-PR = ", round(.data$auc, 3), ")"))
            p <- ggplot2::ggplot(all_curves_data, ggplot2::aes(x = .data$recall, y = .data$precision, color = .data$legend_label)) +
              ggplot2::geom_line(linewidth = 1.2) +
              ggplot2::labs(title = title, x = "Recall", y = "Precision") +
              ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
              theme_optics()
            return(p)
          })

#' Plot a Bland-Altman Plot
#' @param aligned_df A data frame of aligned counts.
#' @param ... Additional arguments.
#' @export
#' @rdname plot_bland_altman
#' @examples
#' \dontrun{
#' erin_csv <- system.file("extdata", 
#' "ice_seals_2025_fl223_C_rgb_irDetectionsTransposed_processed.csv", package = "Optics")
#' model_detections <- read_viame_csv(erin_csv)
#' 
#' erin_truth_csv <- system.file("extdata", 
#' "ice_seals_2025_fl223_C_ir_detections_validated.csv", package = "Optics")
#' truth_detections <- read_viame_csv(erin_truth_csv)
#' 
#' model_counts <- calculate_maxn(model_detections)
#' truth_counts <- calculate_maxn(truth_detections)
#' 
#' aligned_df <- align_counts(
#'  model_counts, 
#'  truth_counts,
#'  by = c("video_id", "category_name"),
#'  model_col = maxn,
#'  truth_col = maxn
#'  )
#' 
#' plot_bland_altman(aligned_df)
#' }
setGeneric("plot_bland_altman", function(aligned_df, ...) standardGeneric("plot_bland_altman"))

#' @rdname plot_bland_altman
#' @export
setMethod("plot_bland_altman", "data.frame",
          function(aligned_df, title = "Bland-Altman Agreement Plot", model_col = NULL) {
            # ... implementation from original function ...
            if (!all(c("model_count", "truth_count") %in% names(aligned_df))) {
              stop("Input data frame must contain 'model_count' and 'truth_count' columns.")
            }
            model_col_quo <- rlang::enquo(model_col)
            plot_data <- aligned_df %>%
              dplyr::mutate(average = (.data$model_count + .data$truth_count) / 2, difference = .data$model_count - .data$truth_count)
            if (rlang::quo_is_null(model_col_quo)) {
              agreement_lines <- plot_data %>%
                dplyr::summarise(mean_diff = mean(.data$difference, na.rm = TRUE),
                                 upper_limit = mean(.data$difference, na.rm = TRUE) + 1.96 * sd(.data$difference, na.rm = TRUE),
                                 lower_limit = mean(.data$difference, na.rm = TRUE) - 1.96 * sd(.data$difference, na.rm = TRUE))
            } else {
              agreement_lines <- plot_data %>%
                dplyr::group_by(!!model_col_quo) %>%
                dplyr::summarise(mean_diff = mean(.data$difference, na.rm = TRUE),
                                 upper_limit = mean(.data$difference, na.rm = TRUE) + 1.96 * sd(.data$difference, na.rm = TRUE),
                                 lower_limit = mean(.data$difference, na.rm = TRUE) - 1.96 * sd(.data$difference, na.rm = TRUE))
            }
            p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$average, y = .data$difference)) +
              ggplot2::geom_point(alpha = 0.6, shape = 16) +
              ggplot2::geom_hline(data = agreement_lines, ggplot2::aes(yintercept = .data$mean_diff), color = "#4E79A7", linetype = "solid", linewidth = 1) +
              ggplot2::geom_hline(data = agreement_lines, ggplot2::aes(yintercept = .data$upper_limit), color = "#af2634", linetype = "dashed", linewidth = 1) +
              ggplot2::geom_hline(data = agreement_lines, ggplot2::aes(yintercept = .data$lower_limit), color = "#af2634", linetype = "dashed", linewidth = 1) +
              ggplot2::labs(title = title, x = "Average of Counts", y = "Difference (Model - Truth)") +
              theme_optics()
            if (!rlang::quo_is_null(model_col_quo)) {
              p <- p + ggplot2::facet_wrap(rlang::quo_get_expr(model_col_quo))
            }
            return(p)
          })

#' Plot a Confusion Matrix
#' @param metrics_df A data frame of metrics.
#' @param ... Additional arguments.
#' @export
#' @rdname plot_confusion_matrix
#' @examples
#' \dontrun{
#' erin_csv <- system.file("extdata", 
#' "ice_seals_2025_fl223_C_rgb_irDetectionsTransposed_processed.csv", package = "Optics")
#' model_detections <- read_viame_csv(erin_csv)
#' 
#' erin_truth_csv <- system.file("extdata", 
#' "ice_seals_2025_fl223_C_ir_detections_validated.csv", package = "Optics")
#' truth_detections <- read_viame_csv(erin_truth_csv)
#' 
#' model_counts <- calculate_maxn(model_detections)
#' truth_counts <- calculate_maxn(truth_detections)
#' 
#' aligned_df <- align_counts(
#'  model_counts, 
#'  truth_counts,
#'  by = c("video_id", "category_name"),
#'  model_col = maxn,
#'  truth_col = maxn
#'  )
#' 
#' metrics_df <- calculate_binary_metrics(aligned_df)
#' 
#' plot_confusion_matrix(metrics_df)
#' }
setGeneric("plot_confusion_matrix", function(metrics_df, ...) standardGeneric("plot_confusion_matrix"))

#' @rdname plot_confusion_matrix
#' @export
setMethod("plot_confusion_matrix", "data.frame",
          function(metrics_df, title = "Confusion Matrix", model_col = NULL) {
            # ... implementation from original function ...
            required_cols <- c("tp", "fp", "fn", "tn")
            if (!all(required_cols %in% names(metrics_df))) {
              stop("Input data frame must contain 'tp', 'fp', 'fn', and 'tn' columns.")
            }
            model_col_quo <- rlang::enquo(model_col)
            cm_data <- metrics_df %>%
              tidyr::pivot_longer(cols = c("tp", "fp", "fn", "tn"), names_to = "metric", values_to = "N") %>%
              dplyr::mutate(Truth = factor(ifelse(.data$metric %in% c("tp", "fn"), "Positive", "Negative"), levels = c("Negative", "Positive")),
                            Prediction = factor(ifelse(.data$metric %in% c("tp", "fp"), "Positive", "Negative"), levels = c("Positive", "Negative")))
            p <- ggplot2::ggplot(data = cm_data, ggplot2::aes(x = .data$Prediction, y = .data$Truth, fill = .data$N)) +
              ggplot2::geom_tile(color = "white") +
              ggplot2::geom_text(ggplot2::aes(label = .data$N), vjust = 1, size = 6, color = "white") +
              ggplot2::scale_fill_gradient(low = "#4E79A7", high = "#af2634") +
              ggplot2::labs(title = title, x = "Predicted Class", y = "True Class") +
              theme_optics(base_size = 14) +
              ggplot2::theme(legend.position = "none")
            if (!rlang::quo_is_null(model_col_quo)) {
              p <- p + ggplot2::facet_wrap(rlang::quo_get_expr(model_col_quo))
            }
            return(p)
          })

#' Plot a Receiver Operating Characteristic (ROC) Curve
#' @param detection_df A data frame of classified detections.
#' @param ... Additional arguments.
#' @export
#' @rdname plot_roc_curve
#' @examples
#' \dontrun{
#' # For Abi's AUV data
#' kwcoco_file <- system.file("extdata", "AUV_viame_test_detections.coco.json", package = "Optics")
#' detections <- read_kwcoco(kwcoco_file)
#' 
#' # This is a placeholder for truth data
#' truth_detections <- detections 
#' 
#' classified_detections <- classify_detections(
#' raw_detections = detections@data,
#' validated_detections = truth_detections@data,
#' detection_id = annotation_id
#' )
#' 
#' plot_roc_curve(classified_detections)
#' }
setGeneric("plot_roc_curve", function(detection_df, ...) standardGeneric("plot_roc_curve"))

#' @rdname plot_roc_curve
#' @export
setMethod("plot_roc_curve", "data.frame",
          function(detection_df, title = "ROC Curve", model_col = NULL) {
            # ... implementation from original function ...
            if (!all(c("score", "status") %in% names(detection_df))) {
              stop("Input data frame must contain 'score' and 'status' columns.")
            }
            model_col_quo <- rlang::enquo(model_col)
            if (rlang::quo_is_null(model_col_quo)) {
              model_col_name <- "model"
              detection_df[[model_col_name]] <- "Model"
              model_col_quo <- rlang::sym(model_col_name)
            }
            roc_list <- detection_df %>%
              dplyr::group_by(!!model_col_quo) %>%
              dplyr::do(roc_obj = pROC::roc(response = .$status, predictor = .$score, levels = c("FP", "TP"), quiet = TRUE))
            plot_data <- roc_list %>%
              dplyr::mutate(auc = roc_obj$auc,
                            legend_label = paste0(!!model_col_quo, " (AUC = ", round(auc, 3), ")"),
                            coords = list(dplyr::tibble(specificity = roc_obj$specificities, sensitivity = roc_obj$sensitivities))) %>%
              tidyr::unnest(cols = c(coords))
            text_data <- plot_data %>% dplyr::distinct(legend_label, auc)
            p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = 1 - .data$specificity, y = .data$sensitivity, color = .data$legend_label)) +
              ggplot2::geom_line(linewidth = 1.2) +
              ggplot2::geom_abline(intercept = 1, slope = 1, linetype = "dashed", color = "grey60") +
              ggplot2::geom_text(data = text_data, ggplot2::aes(x = 0.75, y = 0.25, label = .data$legend_label), show.legend = FALSE) +
              ggplot2::labs(title = title, x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)", color = "Model") +
              theme_optics()
            return(p)
          })

#' Plot a Multi-Class Confusion Matrix Heatmap
#' @param confusion_df A data frame from `calculate_confusion_matrix()`.
#' @param ... Additional arguments.
#' @export
#' @rdname plot_multiclass_confusion_matrix
#' @examples
#' \dontrun{
#' # For Tom & Michael's coral survey
#' kwcoco_file <- system.file("extdata", "AUV_viame_test_detections.coco.json", package = "Optics")
#' detections <- read_kwcoco(kwcoco_file)
#' 
#' # This is a placeholder for truth data
#' truth_detections <- detections 
#' 
#' model_counts <- calculate_maxn(detections)
#' truth_counts <- calculate_maxn(truth_detections)
#' 
#' aligned_df <- align_counts(
#'  model_counts, 
#'  truth_counts,
#'  by = c("video_id", "category_name"),
#'  model_col = maxn,
#'  truth_col = maxn
#'  )
#' 
#' confusion_df <- calculate_confusion_matrix(aligned_df, 
#' group_vars = "video_id", species_col = category_name)
#' 
#' plot_multiclass_confusion_matrix(confusion_df)
#' }
setGeneric("plot_multiclass_confusion_matrix", function(confusion_df, ...) standardGeneric("plot_multiclass_confusion_matrix"))

#' @rdname plot_multiclass_confusion_matrix
#' @export
setMethod("plot_multiclass_confusion_matrix", "data.frame",
          function(confusion_df, title = "Confusion Matrix") {
            # ... implementation from original function ...
            required_cols <- c("Truth", "Prediction", "n")
            if (!all(required_cols %in% names(confusion_df))) {
              stop("Input dataframe must contain columns: Truth, Prediction, n")
            }
            plot_data <- confusion_df %>%
              dplyr::mutate(Truth = as.factor(Truth), Prediction = as.factor(Prediction))
            p <- ggplot(plot_data, aes(x = .data$Prediction, y = .data$Truth, fill = .data$n)) +
              geom_tile(color = "white") +
              geom_text(aes(label = .data$n), color = "white", size = 4) +
              scale_fill_gradient(low = "steelblue", high = "midnightblue", name = "Count") +
              scale_x_discrete(name = "Predicted Class", labels = function(x) stringr::str_wrap(x, width = 15)) +
              scale_y_discrete(name = "True Class", labels = function(x) stringr::str_wrap(x, width = 15)) +
              labs(title = title, subtitle = "Count of co-occurrences in deployments") +
              theme_minimal(base_size = 12) +
              theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
                    axis.ticks = element_blank(), panel.grid = element_blank(),
                    plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
            return(p)
          })

#' Plot Performance Metrics Across Confidence Thresholds
#' @param summary_df A data frame from `summarize_performance_by_threshold()`.
#' @param ... Additional arguments.
#' @export
#' @rdname plot_performance_by_threshold
#' @examples
#' \dontrun{
#' # For Erin's ice seal survey
#' erin_csv <- system.file("extdata", 
#' "ice_seals_2025_fl223_C_rgb_irDetectionsTransposed_processed.csv", package = "Optics")
#' model_detections <- read_viame_csv(erin_csv)
#' 
#' erin_truth_csv <- system.file("extdata", 
#' "ice_seals_2025_fl223_C_ir_detections_validated.csv", package = "Optics")
#' truth_detections <- read_viame_csv(.erin_truth_csv)
#' 
#' performance_summary <- summarize_performance_by_threshold(
#' model_detections = model_detections,
#' truth_detections = truth_detections,
#' by = c("video_id", "category_name")
#' )
#' 
#' plot_performance_by_threshold(performance_summary)
#' }
setGeneric("plot_performance_by_threshold", function(summary_df, ...) standardGeneric("plot_performance_by_threshold"))

#' @rdname plot_performance_by_threshold
#' @export
setMethod("plot_performance_by_threshold", "data.frame",
          function(summary_df, model_col = NULL, title = "Performance by Confidence Threshold") {
            # ... implementation from original function ...
            required_cols <- c("threshold", "precision", "recall", "f1_score")
            if (!all(required_cols %in% names(summary_df))) {
              stop("Input data frame must contain 'threshold', 'precision', 'recall', and 'f1_score' columns.")
            }
            model_col_quo <- rlang::enquo(model_col)
            plot_data <- summary_df %>%
              tidyr::pivot_longer(cols = c("precision", "recall", "f1_score"), names_to = "metric", values_to = "value")
            p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$threshold, y = .data$value, color = .data$metric)) +
              ggplot2::geom_line(linewidth = 1.1) +
              ggplot2::geom_point(size = 2) +
              ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
              ggplot2::labs(title = title, x = "Confidence Threshold", y = "Metric Value", color = "Metric") +
              theme_optics()
            if (!rlang::quo_is_null(model_col_quo)) {
              p <- p + ggplot2::facet_wrap(rlang::quo_get_expr(model_col_quo))
            }
            return(p)
          })
