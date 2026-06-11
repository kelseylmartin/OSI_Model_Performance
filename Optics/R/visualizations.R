#' Plot a Scatterplot of Model vs. Truth Counts
#'
#' Creates a scatter plot comparing model-derived counts to ground truth counts,
#' with a 1:1 line for reference. This is useful for visualizing regression
#' performance on count-based metrics.
#'
#' @param aligned_df A data frame or tibble containing aligned counts, typically
#'   the output of `align_counts()`. Must contain `model_count` and `truth_count`.
#' @param title An optional title for the plot.
#' @return A `ggplot` object, which can be further customized.
#' @export
#' @importFrom ggpubr stat_regline_equation
#' @import ggplot2
#' @examples
#' aligned_data <- dplyr::tibble(
#'   model_count = c(10, 1, 5, 0, 0, 20),
#'   truth_count = c(12, 0, 0, 2, 8, 22)
#' )
#'
#' plot_counts_scatterplot(aligned_data, title = "Model vs. Truth MaxN")
#'
plot_counts_scatterplot <- function(aligned_df, title = "Model vs. Truth Counts") {

  if (!all(c("model_count", "truth_count") %in% names(aligned_df))) {
    stop("Input data frame must contain 'model_count' and 'truth_count' columns.")
  }

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
#' @return A `ggplot` object representing the Bland-Altman plot.
#' @export
#' @import ggplot2
#' @importFrom dplyr mutate
#' @examples
#' aligned_data <- dplyr::tibble(
#'   model_count = c(10, 1, 5, 2, 8, 20),
#'   truth_count = c(12, 0, 2, 2, 8, 22)
#' )
#'
#' plot_bland_altman(aligned_data)
#'
plot_bland_altman <- function(aligned_df, title = "Bland-Altman Agreement Plot") {

  if (!all(c("model_count", "truth_count") %in% names(aligned_df))) {
    stop("Input data frame must contain 'model_count' and 'truth_count' columns.")
  }

  # Calculate differences and averages
  plot_data <- aligned_df %>%
    dplyr::mutate(
      average = (.data$model_count + .data$truth_count) / 2,
      difference = .data$model_count - .data$truth_count
    )

  # Calculate agreement statistics
  mean_diff <- mean(plot_data$difference, na.rm = TRUE)
  sd_diff <- sd(plot_data$difference, na.rm = TRUE)
  upper_limit <- mean_diff + 1.96 * sd_diff
  lower_limit <- mean_diff - 1.96 * sd_diff

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$average, y = .data$difference)) +
    ggplot2::geom_point(alpha = 0.6, shape = 16) +
    ggplot2::geom_hline(yintercept = mean_diff, color = "#4E79A7", linetype = "solid", linewidth = 1) +
    ggplot2::geom_hline(yintercept = c(upper_limit, lower_limit), color = "#af2634", linetype = "dashed", linewidth = 1) +
    ggplot2::labs(
      title = title,
      subtitle = paste0("Mean Difference: ", round(mean_diff, 2)),
      x = "Average of Counts",
      y = "Difference (Model - Truth)"
    ) +
    theme_optics()

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
#' @param title An optional title for the plot.
#' @return A `ggplot` object representing the confusion matrix.
#' @export
#' @import ggplot2
#' @importFrom dplyr tibble
#' @importFrom tidyr pivot_longer
#' @examples
#' metrics <- dplyr::tibble(tp = 10, fp = 2, fn = 3, tn = 85)
#' plot_confusion_matrix(metrics, "Model Performance")
#'
plot_confusion_matrix <- function(metrics_df, title = "Confusion Matrix") {

  required_cols <- c("tp", "fp", "fn", "tn")
  if (!all(required_cols %in% names(metrics_df))) {
    stop("Input data frame must contain 'tp', 'fp', 'fn', and 'tn' columns.")
  }

  # Take the first row if multiple are provided (e.g., from grouped data)
  metrics <- metrics_df[1, ]

  # Create a tibble in the right format for ggplot
  cm_data <- dplyr::tibble(
    Truth = factor(c("Positive", "Positive", "Negative", "Negative"), levels = c("Negative", "Positive")),
    Prediction = factor(c("Positive", "Negative", "Positive", "Negative"), levels = c("Positive", "Negative")),
    N = c(metrics$tp, metrics$fn, metrics$fp, metrics$tn)
  )

  p <- ggplot2::ggplot(data = cm_data, ggplot2::aes(x = .data$Prediction, y = .data$Truth, fill = .data$N)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = .data$N), vjust = 1, size = 6, color = "white") +
    ggplot2::scale_fill_gradient(low = "#4E79A7", high = "#af2634") +
    ggplot2::labs(
      title = title,
      x = "Predicted Class",
      y = "True Class"
    ) +
    theme_optics(base_size = 14) +
    ggplot2::theme(legend.position = "none")

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
#' @param title An optional title for the plot.
#' @return A `ggplot` object representing the ROC curve.
#' @export
#' @import ggplot2
#' @importFrom pROC roc ggroc
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
plot_roc_curve <- function(detection_df, title = "ROC Curve") {

  if (!all(c("score", "status") %in% names(detection_df))) {
    stop("Input data frame must contain 'score' and 'status' columns.")
  }

  # Create the ROC object
  roc_obj <- pROC::roc(
    response = ifelse(detection_df$status == "TP", 1, 0),
    predictor = detection_df$score,
    levels = c(0, 1), # 0=FP (negative), 1=TP (positive)
    quiet = TRUE
  )

  # Extract AUC for annotating the plot
  auc_value <- round(roc_obj$auc, 3)

  p <- pROC::ggroc(roc_obj, colour = "#4E79A7", linewidth = 1.2) +
    ggplot2::geom_abline(intercept = 1, slope = 1, linetype = "dashed", color = "grey60") +
    ggplot2::annotate("text", x = 0.4, y = 0.2, label = paste("AUC =", auc_value), size = 5) +
    ggplot2::labs(title = title, x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)") +
    theme_optics()

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
plot_pr_curve <- function(detection_df, title = "Precision-Recall Curve") {

  if (!all(c("score", "status") %in% names(detection_df))) {
    stop("Input data frame must contain 'score' and 'status' columns.")
  }

  # Separate scores for true positives (positive class) and false positives (negative class)
  tp_scores <- detection_df %>% dplyr::filter(.data$status == "TP") %>% dplyr::pull(.data$score)
  fp_scores <- detection_df %>% dplyr::filter(.data$status == "FP") %>% dplyr::pull(.data$score)

  # Calculate PR curve using PRROC
  pr_obj <- PRROC::pr.curve(scores.class0 = fp_scores, scores.class1 = tp_scores, curve = TRUE)

  # Extract curve data and AUC for plotting
  pr_curve_data <- as.data.frame(pr_obj$curve)
  names(pr_curve_data) <- c("recall", "precision", "threshold")
  auc_pr <- round(pr_obj$auc.integral, 3)

  p <- ggplot2::ggplot(pr_curve_data, ggplot2::aes(x = .data$recall, y = .data$precision)) +
    ggplot2::geom_line(colour = "#4E79A7", linewidth = 1.2) +
    ggplot2::annotate("text", x = 0.7, y = 0.2, label = paste("AUC-PR =", auc_pr), size = 5) +
    ggplot2::labs(title = title, x = "Recall", y = "Precision") +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    theme_optics()

  return(p)
}