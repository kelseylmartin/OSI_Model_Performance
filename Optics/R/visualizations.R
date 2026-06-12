#' Plot a Scatterplot of Model vs. Truth Counts
#'
#' Creates a scatter plot comparing model-derived counts to ground truth counts,
#' with a 1:1 line for reference. This is useful for visualizing regression
#' performance on count-based metrics.
#'
#' @param aligned_df A data frame or tibble containing aligned counts, typically
#'   the output of `align_counts()`. Must contain `model_count` and `truth_count`.
#' @param title An optional title for the plot.
#' @param model_col An optional unquoted column name that identifies the model
#'   or group. If provided, a separate plot panel will be generated for each
#'   unique value in this column.
#' @return A `ggplot` object, which can be further customized.
#' @export
#' @importFrom ggpubr stat_regline_equation
#' @import ggplot2
#' @importFrom rlang enquo quo_is_null
#' @examples
#' aligned_data <- dplyr::tibble(
#'   model_count = c(10, 1, 5, 0, 0, 20),
#'   truth_count = c(12, 0, 0, 2, 8, 22)
#' )
#'
#' plot_counts_scatterplot(aligned_data, title = "Model vs. Truth MaxN")
#' 
#' # Example with multiple models
#' aligned_multi <- dplyr::tibble(
#'   model_count = c(10, 1, 5, 12, 2, 4),
#'   truth_count = c(12, 0, 2, 11, 3, 4),
#'   model_name = rep(c("Model A", "Model B"), each = 3)
#' )
#' plot_counts_scatterplot(aligned_multi, model_col = model_name)
#'
plot_counts_scatterplot <- function(aligned_df, title = "Model vs. Truth Counts", model_col = NULL) {

  if (!all(c("model_count", "truth_count") %in% names(aligned_df))) {
    stop("Input data frame must contain 'model_count' and 'truth_count' columns.")
  }
  
  model_col_quo <- rlang::enquo(model_col)

  p <- ggplot2::ggplot(aligned_df, ggplot2::aes(x = .data$truth_count, y = .data$model_count)) +
    ggplot2::geom_point(alpha = 0.6, shape = 16) +
    # Add a 1:1 reference line
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    # Add the regression equation and R-squared value
    ggpubr::stat_regline_equation(label.y.npc = 0.9, label.x.npc = 0.05, size = 4) +
    ggpubr::stat_regline_equation(label.y.npc = 0.8, label.x.npc = 0.05, aes(label = after_stat(rr.label)), size = 4) +
    ggplot2::labs(
      title = title,
      x = "Ground Truth Count",
      y = "Model Predicted Count"
    ) +
    theme_optics(base_size = 14) +
    ggplot2::coord_equal()
    
  if (!rlang::quo_is_null(model_col_quo)) {
    p <- p + ggplot2::facet_wrap(rlang::quo_get_expr(model_col_quo))
  }

  return(p)
}

#' Plot a Precision-Recall (PR) Curve
#'
#' Generates a PR curve to evaluate model performance, showing the trade-off
#' between precision and recall across all confidence thresholds.
#'
#' This function uses the `PRROC` package to calculate the curve and the Area
#' Under the Curve (AUC-PR). It requires a data frame of model detections that
#' have been classified as either true positives (TP) or false positives (FP).
#'
#' @param detection_df A data frame where each row is a model detection. Must
#'   contain a `score` column with the model's confidence score, and a `status`
#'   column (e.g., "TP" or "FP").
#' @param model_col An optional unquoted column name that identifies the model
#'   or group. If provided, a separate PR curve will be generated for each
#'   unique value in this column.
#' @param title An optional title for the plot.
#' @return A `ggplot` object representing the PR curve.
#' @export
#' @import ggplot2
#' @importFrom PRROC pr.curve
#' @importFrom dplyr filter
#' @examples
#' # Create sample data of classified detections
#' pr_data <- dplyr::tibble(
#'   score = c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2),
#'   status = c("TP", "TP", "FP", "TP", "FP", "FP", "TP", "FP")
#' )
#'
#' plot_pr_curve(pr_data)
#'
#' # Example with multiple models
#' pr_data_multi <- dplyr::tibble(
#'   score = runif(200),
#'   status = sample(c("TP", "FP"), 200, replace = TRUE),
#'   model_name = rep(c("Model A", "Model B"), each = 100)
#' )
#' plot_pr_curve(pr_data_multi, model_col = model_name)
#'
plot_pr_curve <- function(detection_df, model_col = NULL, title = "Precision-Recall Curve") {

  if (!all(c("score", "status") %in% names(detection_df))) {
    stop("Input data frame must contain 'score' and 'status' columns.")
  }

  model_col_quo <- rlang::enquo(model_col)

  # If no model column is provided, treat as a single group
  if (rlang::quo_is_null(model_col_quo)) {
    model_col_name <- "model" # A dummy name
    detection_df[[model_col_name]] <- "Model"
    model_col_quo <- rlang::sym(model_col_name) # Update quosure to point to the new column
  }

  # Group by the model column and calculate PR curve for each group
  all_curves_data <- detection_df %>%
    dplyr::group_by(!!model_col_quo) %>%
    dplyr::do({
      df_group <- .
      tp_scores <- df_group %>% dplyr::filter(.data$status == "TP") %>% dplyr::pull(.data$score)
      fp_scores <- df_group %>% dplyr::filter(.data$status == "FP") %>% dplyr::pull(.data$score)

      # Handle cases where a group has no TPs or FPs
      if (length(tp_scores) == 0 && length(fp_scores) == 0) {
        return(NULL)
      }

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
}

#' Plot a Bland-Altman Plot for Agreement
#'
#' Creates a Bland-Altman plot to visualize the agreement between two
#' quantitative measurements (model vs. truth counts).
#'
#' @param aligned_df A data frame or tibble containing aligned counts, typically
#'   the output of `align_counts()`. Must contain `model_count` and `truth_count`.
#' @param title An optional title for the plot.
#' @param model_col An optional unquoted column name that identifies the model
#'   or group. If provided, a separate plot panel will be generated for each
#'   unique value in this column.
#' @return A `ggplot` object representing the Bland-Altman plot.
#' @export
#' @import ggplot2
#' @importFrom dplyr mutate
#' @importFrom rlang enquo quo_is_null
#' @examples
#' aligned_data <- dplyr::tibble(
#'   model_count = c(10, 1, 5, 2, 8, 20),
#'   truth_count = c(12, 0, 2, 2, 8, 22)
#' )
#'
#' plot_bland_altman(aligned_data)
#'
plot_bland_altman <- function(aligned_df, title = "Bland-Altman Agreement Plot", model_col = NULL) {

  if (!all(c("model_count", "truth_count") %in% names(aligned_df))) {
    stop("Input data frame must contain 'model_count' and 'truth_count' columns.")
  }
  
  model_col_quo <- rlang::enquo(model_col)

  # Calculate differences and averages
  plot_data <- aligned_df %>%
    dplyr::mutate(
      average = (.data$model_count + .data$truth_count) / 2,
      difference = .data$model_count - .data$truth_count
    )

  # Calculate agreement statistics (grouped if model_col is provided)
  if (rlang::quo_is_null(model_col_quo)) {
    agreement_lines <- plot_data %>%
      dplyr::summarise(
        mean_diff = mean(.data$difference, na.rm = TRUE),
        upper_limit = mean(.data$difference, na.rm = TRUE) + 1.96 * sd(.data$difference, na.rm = TRUE),
        lower_limit = mean(.data$difference, na.rm = TRUE) - 1.96 * sd(.data$difference, na.rm = TRUE)
      )
  } else {
    agreement_lines <- plot_data %>%
      dplyr::group_by(!!model_col_quo) %>%
      dplyr::summarise(
        mean_diff = mean(.data$difference, na.rm = TRUE),
        upper_limit = mean(.data$difference, na.rm = TRUE) + 1.96 * sd(.data$difference, na.rm = TRUE),
        lower_limit = mean(.data$difference, na.rm = TRUE) - 1.96 * sd(.data$difference, na.rm = TRUE)
      )
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
}

#' Plot a Confusion Matrix
#'
#' Generates a heatmap visualization of a confusion matrix from performance
#' metrics (TP, FP, TN, FN).
#'
#' @param metrics_df A data frame containing performance metrics, typically the
#'   output of `calculate_binary_metrics()`. Must contain `tp`, `fp`, `fn`, and `tn`.
#'   If the data frame is grouped, only the first group's metrics will be plotted.
#' @param model_col An optional unquoted column name that identifies the model
#'   or group. If provided, a separate plot panel will be generated for each
#'   unique value in this column.
#' @param title An optional title for the plot.
#' @return A `ggplot` object representing the confusion matrix.
#' @export
#' @import ggplot2
#' @importFrom dplyr tibble
#' @importFrom tidyr pivot_longer
#' @importFrom rlang enquo quo_is_null .data
#' @examples
#' metrics <- dplyr::tibble(tp = 10, fp = 2, fn = 3, tn = 85)
#' plot_confusion_matrix(metrics, "Model Performance")
#'
plot_confusion_matrix <- function(metrics_df, title = "Confusion Matrix", model_col = NULL) {

  required_cols <- c("tp", "fp", "fn", "tn")
  if (!all(required_cols %in% names(metrics_df))) {
    stop("Input data frame must contain 'tp', 'fp', 'fn', and 'tn' columns.")
  }
  
  model_col_quo <- rlang::enquo(model_col)

  # Reshape data for plotting
  cm_data <- metrics_df %>%
    tidyr::pivot_longer(
      cols = c("tp", "fp", "fn", "tn"),
      names_to = "metric",
      values_to = "N"
    ) %>%
    dplyr::mutate(
      Truth = factor(ifelse(.data$metric %in% c("tp", "fn"), "Positive", "Negative"), levels = c("Negative", "Positive")),
      Prediction = factor(ifelse(.data$metric %in% c("tp", "fp"), "Positive", "Negative"), levels = c("Positive", "Negative"))
    )

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
}

#' Plot a Receiver Operating Characteristic (ROC) Curve
#'
#' Generates an ROC curve to evaluate model performance across all confidence
#' thresholds.
#'
#' This function requires a data frame of model detections that have been
#' classified as either true positives (TP) or false positives (FP). It uses
#' the `pROC` package to calculate the curve and the Area Under the Curve (AUC).
#'
#' @param detection_df A data frame where each row is a model detection. Must
#'   contain a `score` column with the model's confidence score, and a `status`
#'   column (e.g., "TP" or "FP") indicating if the detection was a true or false
#'   positive.
#' @param model_col An optional unquoted column name that identifies the model
#'   or group. If provided, a separate ROC curve will be generated for each
#'   unique value in this column.
#' @param title An optional title for the plot.
#' @return A `ggplot` object representing the ROC curve.
#' @export
#' @import ggplot2
#' @importFrom pROC roc
#' @importFrom rlang enquo quo_is_null .data
#' @examples
#' # Create sample data of classified detections
#' roc_data <- dplyr::tibble(
#'   score = c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2),
#'   # TPs have high scores, FPs have lower scores
#'   status = c("TP", "TP", "FP", "TP", "FP", "FP", "TP", "FP")
#' )
#'
#' plot_roc_curve(roc_data)
#'
plot_roc_curve <- function(detection_df, title = "ROC Curve", model_col = NULL) {

  if (!all(c("score", "status") %in% names(detection_df))) {
    stop("Input data frame must contain 'score' and 'status' columns.")
  }
  
  model_col_quo <- rlang::enquo(model_col)

  # If no model column is provided, create a dummy one for grouping
  if (rlang::quo_is_null(model_col_quo)) {
    model_col_name <- "model"
    detection_df[[model_col_name]] <- "Model"
    model_col_quo <- rlang::sym(model_col_name)
  }

  # Calculate ROC for each model
  roc_list <- detection_df %>%
    dplyr::group_by(!!model_col_quo) %>%
    dplyr::do(
      roc_obj = pROC::roc(
        response = .$status,
        predictor = .$score,
        levels = c("FP", "TP"), # FP is the negative class, TP is the positive
        quiet = TRUE
      )
    )

  # Prepare data for plotting
  plot_data <- roc_list %>%
    dplyr::mutate(
      auc = roc_obj$auc,
      legend_label = paste0(!!model_col_quo, " (AUC = ", round(auc, 3), ")"),
      coords = list(dplyr::tibble(
        specificity = roc_obj$specificities,
        sensitivity = roc_obj$sensitivities
      ))
    ) %>%
    tidyr::unnest(cols = c(coords))

  # Create a separate data frame for the text annotations
  text_data <- plot_data %>%
    dplyr::distinct(legend_label, auc)

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = 1 - .data$specificity, y = .data$sensitivity, color = .data$legend_label)) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::geom_abline(intercept = 1, slope = 1, linetype = "dashed", color = "grey60") +
    ggplot2::geom_text(data = text_data, ggplot2::aes(x = 0.75, y = 0.25, label = .data$legend_label), show.legend = FALSE) +
    ggplot2::labs(title = title, x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)", color = "Model") +
    theme_optics()

  return(p)
}