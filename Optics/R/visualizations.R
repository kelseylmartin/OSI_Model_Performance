#' @include theme.R
NULL

#' Plot a Scatterplot of Model vs. Truth Counts
#' @param aligned_df A data frame containing aligned counts.
#' @param ... Additional arguments.
#' @export
#' @rdname plot_counts_scatterplot
#' @examples
#' # Example using GFISHER data.
#' model_csv_data <- c("1,video1,10,100,100,200,200,1,0.95,\"Gadus morhua\",1")
#' truth_csv_data <- c("1,video1,10,100,100,200,200,1,1.0,\"Gadus morhua\",1")
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
#'  )
#' 
#' plot_counts_scatterplot(aligned_df)
#' 
#' unlink(model_csv_path)
#' unlink(truth_csv_path)
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
            if (!requireNamespace("ggpubr", quietly = TRUE)) {
              stop("Package 'ggpubr' is required for plot_counts_scatterplot(). Please install it.", call. = FALSE)
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
#' @param title Character. Plot title.
#' @param model_col Symbol or character for model column.
#' @rdname plot_pr_curve
#' @examples
#' # Example using Erin's ice seal data.
#' raw_csv_data <- c("1,video1,10,100,100,200,200,1,0.95,\"ringed_seal\",1")
#' validated_csv_data <- c("1,video1,10,100,100,200,200,1,1.0,\"ringed_seal\",1")
#' raw_csv_path <- tempfile(fileext = ".csv")
#' validated_csv_path <- tempfile(fileext = ".csv")
#' writeLines(c("# h1", "# h2", raw_csv_data), raw_csv_path)
#' writeLines(c("# h1", "# h2", validated_csv_data), validated_csv_path)
#'
#' raw_detections <- read_viame_csv(raw_csv_path)
#' validated_detections <- read_viame_csv(validated_csv_path)
#'
#' classified_detections <- classify_detections(
#'   raw_detections = raw_detections@data,
#'   validated_detections = validated_detections@data,
#'   detection_id = annotation_id
#' )
#'
#' plot_pr_curve(classified_detections)
#'
#' unlink(raw_csv_path)
#' unlink(validated_csv_path)
setGeneric("plot_pr_curve", function(detection_df, ...) standardGeneric("plot_pr_curve"))

#' @rdname plot_pr_curve
#' @export
setMethod("plot_pr_curve", "data.frame",
          function(detection_df, model_col = NULL, title = "Precision-Recall Curve") {
            # ... implementation from original function ...
            if (!all(c("score", "status") %in% names(detection_df))) {
              stop("Input data frame must contain 'score' and 'status' columns.")
            }
            if (!requireNamespace("PRROC", quietly = TRUE)) {
              stop("Package 'PRROC' is required for plot_pr_curve(). Please install it.", call. = FALSE)
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
#' @param title Character. Plot title.
#' @param model_col Symbol or character for model column.
#' @rdname plot_bland_altman
#' @examples
#' # Example using Erin's ice seal data.
#' model_csv_data <- c("1,video1,10,100,100,200,200,1,0.95,\"ringed_seal\",1")
#' truth_csv_data <- c("1,video1,10,100,100,200,200,1,1.0,\"ringed_seal\",1")
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
#' plot_bland_altman(aligned_df)
#'
#' unlink(model_csv_path)
#' unlink(truth_csv_path)
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
#' @param title Character. Plot title.
#' @param model_col Symbol or character for model column.
#' @rdname plot_confusion_matrix
#' @examples
#' # Example using Erin's ice seal data.
#' model_csv_data <- c("1,video1,10,100,100,200,200,1,0.95,\"ringed_seal\",1")
#' truth_csv_data <- c("1,video1,10,100,100,200,200,1,1.0,\"ringed_seal\",1")
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
#' metrics_df <- calculate_binary_metrics(aligned_df, total_comparisons = 2)
#'
#' plot_confusion_matrix(metrics_df)
#'
#' unlink(model_csv_path)
#' unlink(truth_csv_path)
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
#' @param title Character. Plot title.
#' @param model_col Symbol or character for model column.
#' @rdname plot_roc_curve
#' @examples
#' # Example using Abi's AUV data.
#' raw_csv_data <- c("1,video1,10,100,100,200,200,1,0.95,\"sea_star\",1")
#' validated_csv_data <- c("1,video1,10,100,100,200,200,1,1.0,\"sea_star\",1")
#' raw_csv_path <- tempfile(fileext = ".csv")
#' validated_csv_path <- tempfile(fileext = ".csv")
#' writeLines(c("# h1", "# h2", raw_csv_data), raw_csv_path)
#' writeLines(c("# h1", "# h2", validated_csv_data), validated_csv_path)
#'
#' raw_detections <- read_viame_csv(raw_csv_path)
#' validated_detections <- read_viame_csv(validated_csv_path)
#'
#' classified_detections <- classify_detections(
#'   raw_detections = raw_detections@data,
#'   validated_detections = validated_detections@data,
#'   detection_id = annotation_id
#' )
#'
#' plot_roc_curve(classified_detections)
#'
#' unlink(raw_csv_path)
#' unlink(validated_csv_path)
setGeneric("plot_roc_curve", function(detection_df, ...) standardGeneric("plot_roc_curve"))

#' @rdname plot_roc_curve
#' @export
setMethod("plot_roc_curve", "data.frame",
          function(detection_df, title = "ROC Curve", model_col = NULL) {
            # ... implementation from original function ...
            if (!all(c("score", "status") %in% names(detection_df))) {
              stop("Input data frame must contain 'score' and 'status' columns.")
            }
            if (!requireNamespace("pROC", quietly = TRUE)) {
              stop("Package 'pROC' is required for plot_roc_curve(). Please install it.", call. = FALSE)
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
#' @param title Character. Plot title.
#' @rdname plot_multiclass_confusion_matrix
#' @examples
#' # Example using Tom & Michael's coral survey data.
#' model_csv_data <- c("1,video1,10,100,100,200,200,1,0.95,\"Acropora\",1")
#' truth_csv_data <- c("1,video1,10,100,100,200,200,1,1.0,\"Pocillopora\",1")
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
#' confusion_df <- calculate_confusion_matrix(aligned_df,
#'  group_vars = "video_id", species_col = category_name)
#'
#' plot_multiclass_confusion_matrix(confusion_df)
#'
#' unlink(model_csv_path)
#' unlink(truth_csv_path)
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
#' @param title Character. Plot title.
#' @param model_col Symbol or character for model column.
#' @rdname plot_performance_by_threshold
#' @examples
#' # Example using Erin's ice seal data.
#' model_csv_data <- c("1,video1,10,100,100,200,200,1,0.95,\"ringed_seal\",1")
#' truth_csv_data <- c("1,video1,10,100,100,200,200,1,1.0,\"ringed_seal\",1")
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
#'
#' plot_performance_by_threshold(performance_summary)
#'
#' unlink(model_csv_path)
#' unlink(truth_csv_path)
setGeneric("plot_performance_by_threshold", function(summary_df, ...) standardGeneric("plot_performance_by_threshold"))

#' @rdname plot_performance_by_threshold
#' @export
setMethod("plot_performance_by_threshold", "data.frame",
          function(summary_df, model_col = NULL, title = "Performance by Confidence Threshold") {
            # ... implementation from original function ...
            if (!"score" %in% names(summary_df) && "threshold" %in% names(summary_df)) {
              summary_df$score <- summary_df$threshold
            }
            required_cols <- c("score", "precision", "recall", "f1_score")
            if (!all(required_cols %in% names(summary_df))) {
              stop("Input data frame must contain 'score', 'precision', 'recall', and 'f1_score' columns.")
            }
            model_col_quo <- rlang::enquo(model_col)
            plot_data <- summary_df %>%
              tidyr::pivot_longer(cols = c("precision", "recall", "f1_score"), names_to = "metric", values_to = "value")
            score_breaks <- sort(unique(summary_df$score[is.finite(summary_df$score)]))
            best_row <- summary_df %>%
              dplyr::filter(.data$f1_score == max(.data$f1_score, na.rm = TRUE)) %>%
              dplyr::arrange(dplyr::desc(.data$score)) %>%
              dplyr::slice(1)
            p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$score, y = .data$value, color = .data$metric)) +
              ggplot2::geom_line(linewidth = 1.1) +
              ggplot2::geom_point(size = 2) +
              ggplot2::scale_x_continuous(breaks = score_breaks) +
              ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
              ggplot2::labs(title = title, x = "Confidence Threshold", y = "Metric Value", color = "Metric") +
              theme_optics() +
              ggplot2::geom_vline(xintercept = best_row$score[[1]], linetype = "dashed", color = "#E15759")
            if (!rlang::quo_is_null(model_col_quo)) {
              p <- p + ggplot2::facet_wrap(rlang::quo_get_expr(model_col_quo))
            }
            return(p)
          })

#' Plot a ScalPred F1 Curve Across Confidence Thresholds
#'
#' @param summary_df A data frame from `calculate_scalpred_metrics()`.
#' @param ... Additional arguments.
#' @return A `ggplot` object.
#' @export
#' @rdname plot_scalpred_f1_curve
setGeneric("plot_scalpred_f1_curve", function(summary_df, ...) standardGeneric("plot_scalpred_f1_curve"))

#' @param model_col Optional unquoted model column for faceting.
#' @param title Plot title.
#' @rdname plot_scalpred_f1_curve
#' @export
setMethod("plot_scalpred_f1_curve", "data.frame",
          function(summary_df, model_col = NULL, title = "ScalPred F1 by Threshold") {
            if (!"score" %in% names(summary_df) && "threshold" %in% names(summary_df)) {
              summary_df$score <- summary_df$threshold
            }
            required_cols <- c("score", "f1_score")
            if (!all(required_cols %in% names(summary_df))) {
              stop("Input data frame must contain 'score' and 'f1_score' columns.")
            }

            model_col_quo <- rlang::enquo(model_col)
            score_breaks <- sort(unique(summary_df$score[is.finite(summary_df$score)]))
            best_row <- summary_df %>%
              dplyr::filter(.data$f1_score == max(.data$f1_score, na.rm = TRUE)) %>%
              dplyr::arrange(dplyr::desc(.data$score)) %>%
              dplyr::slice(1)
            p <- ggplot2::ggplot(summary_df, ggplot2::aes(x = .data$score, y = .data$f1_score)) +
              ggplot2::geom_line(linewidth = 1.1, color = "#4E79A7") +
              ggplot2::geom_point(size = 2, color = "#4E79A7") +
              ggplot2::scale_x_continuous(breaks = score_breaks) +
              ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
              ggplot2::labs(title = title, x = "Confidence Threshold", y = "F1 Score") +
              theme_optics() +
              ggplot2::geom_vline(xintercept = best_row$score[[1]], linetype = "dashed", color = "#E15759")

            if (!rlang::quo_is_null(model_col_quo)) {
              p <- p + ggplot2::facet_wrap(rlang::quo_get_expr(model_col_quo))
            }
            p
          })

#' Plot a ScalPred Precision-Recall Curve
#'
#' @param summary_df A data frame from `calculate_scalpred_metrics()`.
#' @param ... Additional arguments.
#' @return A `ggplot` object.
#' @export
#' @rdname plot_scalpred_pr_curve
setGeneric("plot_scalpred_pr_curve", function(summary_df, ...) standardGeneric("plot_scalpred_pr_curve"))

#' @param model_col Optional unquoted model column for faceting.
#' @param title Plot title.
#' @rdname plot_scalpred_pr_curve
#' @export
setMethod("plot_scalpred_pr_curve", "data.frame",
          function(summary_df, model_col = NULL, title = "ScalPred Precision-Recall Curve") {
            required_cols <- c("precision", "recall")
            if (!all(required_cols %in% names(summary_df))) {
              stop("Input data frame must contain 'precision' and 'recall' columns.")
            }

            model_col_quo <- rlang::enquo(model_col)
            plot_df <- summary_df %>%
              dplyr::arrange(.data$recall, .data$precision)

            p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$recall, y = .data$precision)) +
              ggplot2::geom_path(linewidth = 1.1, color = "#59A14F") +
              ggplot2::geom_point(size = 2, color = "#59A14F") +
              ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
              ggplot2::labs(title = title, x = "Recall", y = "Precision") +
              theme_optics()

            if (!rlang::quo_is_null(model_col_quo)) {
              p <- p + ggplot2::facet_wrap(rlang::quo_get_expr(model_col_quo))
            }
            p
          })
