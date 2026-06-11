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