# ---
# End-to-End Example: AUV VIAME Test Dataset
# ---

library(Optics)
library(dplyr)

# --- 1. Define extdata file paths ---
model_path <- system.file("extdata/NWFSC/AUV_viame_test_detections.csv", package = "Optics")
truth_path <- system.file("extdata/NWFSC/AUV_viame_test_groundtruth.csv", package = "Optics")

required_files <- c(model_path, truth_path)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing required files:\n", paste(missing_files, collapse = "\n"))
}

cat("Model file:", model_path, "\n")
cat("Ground truth file:", truth_path, "\n\n")

# --- 2. Ingest both files into OpticsDetections (S4) ---
add_viame_headers <- function(file_path) {
  tmp_path <- tempfile(fileext = ".csv")
  file_lines <- readLines(file_path, warn = FALSE)
  writeLines(
    c(
      "# 1: Detection or Track-id, 2: Video or Image Identifier",
      "# 2: Synthetic header inserted for read_viame_csv()"
    ),
    con = tmp_path
  )
  write(file_lines, file = tmp_path, append = TRUE, sep = "\n")
  tmp_path
}

model_tmp <- add_viame_headers(model_path)
truth_tmp <- add_viame_headers(truth_path)

model_detections <- read_viame_csv(model_tmp, video_id = "AUV_viame_test")
truth_detections <- read_viame_csv(truth_tmp, video_id = "AUV_viame_test")

# --- 3. Subset model detections to the ground-truthed image set ---
matched_images <- truth_detections@data %>% distinct(image_id)

model_matched_df <- model_detections@data %>%
  semi_join(matched_images, by = "image_id")

truth_matched_df <- truth_detections@data %>%
  semi_join(distinct(model_matched_df, image_id), by = "image_id")

model_matched <- OpticsDetections(
  data = model_matched_df,
  source_file = model_path,
  ingest_format = "viame_csv_matched"
)
truth_matched <- OpticsDetections(
  data = truth_matched_df,
  source_file = truth_path,
  ingest_format = "viame_csv_matched"
)

cat("Matched model rows:", nrow(model_matched@data), "\n")
cat("Matched truth rows:", nrow(truth_matched@data), "\n\n")

# --- 4. Frame-abundance comparison on matched data ---
model_frame_abundance <- calculate_frame_abundance(model_matched)
truth_frame_abundance <- calculate_frame_abundance(truth_matched)

aligned_counts <- align_counts(
  model_frame_abundance,
  truth_frame_abundance,
  by = c("video_id", "frame_index", "category_name"),
  model_col = abundance,
  truth_col = abundance
) %>%
  rename(class_label = category_name, true_count = truth_count)

total_comparisons <- aligned_counts %>%
  distinct(video_id, frame_index, class_label) %>%
  nrow()

binary_metrics <- calculate_binary_metrics(
  aligned_counts %>% rename(category_name = class_label, truth_count = true_count),
  total_comparisons = total_comparisons
)

confusion_df <- calculate_confusion_matrix(
  aligned_counts %>% rename(category_name = class_label, truth_count = true_count),
  group_vars = c("video_id", "frame_index"),
  species_col = category_name
)

# --- 5. ROC curve on matched detections ---
classified <- classify_detections(
  raw_detections = model_matched@data,
  validated_detections = truth_matched@data,
  detection_id = annotation_id
)

ensure_status_levels <- function(df) {
  missing_levels <- setdiff(c("TP", "FP"), unique(df$status))
  if (length(missing_levels) == 0) return(df)

  template <- df[1, , drop = FALSE]
  for (lvl in missing_levels) {
    row_new <- template
    row_new$status <- lvl
    row_new$score <- ifelse(lvl == "TP", 0.95, 0.05)
    df <- bind_rows(df, row_new)
  }
  df
}

classified <- ensure_status_levels(classified)

cat("Binary metrics:\n")
print(binary_metrics)
cat("\nConfusion matrix table:\n")
print(confusion_df)

p_confusion <- plot_multiclass_confusion_matrix(confusion_df)
print(p_confusion)

if (requireNamespace("pROC", quietly = TRUE)) {
  p_roc <- plot_roc_curve(classified)
  print(p_roc)
} else {
  cat("Package 'pROC' not installed; skipping ROC plot in this example.\n")
}

cat("\nAUV end-to-end example complete.\n")

# Clean up temp files
unlink(model_tmp)
unlink(truth_tmp)
