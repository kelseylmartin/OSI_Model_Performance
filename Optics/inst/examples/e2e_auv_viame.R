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

# --- 2. Ingest model and ground truth into OpticsDetections objects ---
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

model_a_raw <- read_viame_csv(model_tmp, video_id = "AUV_viame_test")
truth_raw <- read_viame_csv(truth_tmp, video_id = "AUV_viame_test")

# --- 3. Restrict analysis to the GT image subset ---
matched_images <- truth_raw@data %>% distinct(image_id)

model_a_df <- model_a_raw@data %>%
  semi_join(matched_images, by = "image_id")
truth_df <- truth_raw@data %>%
  semi_join(distinct(model_a_df, image_id), by = "image_id")

# Build a second model example (model comparison) from a stricter score cutoff.
score_cutoff <- stats::quantile(model_a_df$score, probs = 0.75, na.rm = TRUE)
model_b_df <- model_a_df %>% filter(score >= score_cutoff)

model_a <- OpticsDetections(model_a_df, model_path, "viame_csv_model_a_matched")
model_b <- OpticsDetections(model_b_df, model_path, "viame_csv_model_b_matched")
truth <- OpticsDetections(truth_df, truth_path, "viame_csv_truth_matched")

cat("Matched model A rows:", nrow(model_a@data), "\n")
cat("Matched model B rows:", nrow(model_b@data), "\n")
cat("Matched truth rows:", nrow(truth@data), "\n\n")

# --- 4. Frame-abundance metrics and model-vs-model comparison ---
model_a_frame_abundance <- calculate_frame_abundance(model_a)
model_b_frame_abundance <- calculate_frame_abundance(model_b)
truth_frame_abundance <- calculate_frame_abundance(truth)

aligned_a <- align_counts(
  model_a_frame_abundance,
  truth_frame_abundance,
  by = c("video_id", "frame_index", "category_name"),
  model_col = abundance,
  truth_col = abundance
) %>% mutate(model_name = "model_a")

aligned_b <- align_counts(
  model_b_frame_abundance,
  truth_frame_abundance,
  by = c("video_id", "frame_index", "category_name"),
  model_col = abundance,
  truth_col = abundance
) %>% mutate(model_name = "model_b")

aligned_both <- bind_rows(aligned_a, aligned_b) %>%
  rename(class_label = category_name, true_count = truth_count)

# Additional metric utility demo
aligned_a_density <- calculate_density(aligned_a, count_col = model_count, area = 1)

all_groups <- bind_rows(
  distinct(aligned_a, video_id, frame_index, category_name),
  distinct(aligned_b, video_id, frame_index, category_name)
)
total_comparisons <- nrow(distinct(all_groups))

metrics_a <- calculate_binary_metrics(aligned_a, total_comparisons = total_comparisons)
metrics_b <- calculate_binary_metrics(aligned_b, total_comparisons = total_comparisons)
metrics_both <- bind_rows(
  mutate(metrics_a, model_name = "model_a"),
  mutate(metrics_b, model_name = "model_b")
)

print(metrics_both)

# --- 5. Threshold summaries for each model ---
perf_a <- summarize_performance_by_threshold(
  model_detections = model_a,
  truth_detections = truth,
  by = c("video_id", "frame_index", "category_name"),
  metric_function = calculate_frame_abundance
) %>% mutate(model_name = "model_a")

perf_b <- summarize_performance_by_threshold(
  model_detections = model_b,
  truth_detections = truth,
  by = c("video_id", "frame_index", "category_name"),
  metric_function = calculate_frame_abundance
) %>% mutate(model_name = "model_b")

perf_both <- bind_rows(perf_a, perf_b)

# --- 6. Detection-level classification + reviewer effort ---
classified_a <- classify_detections(
  raw_detections = model_a@data,
  validated_detections = truth@data,
  detection_id = annotation_id
) %>% mutate(model_name = "model_a")

classified_b <- classify_detections(
  raw_detections = model_b@data,
  validated_detections = truth@data,
  detection_id = annotation_id
) %>% mutate(model_name = "model_b")

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

classified_both <- bind_rows(classified_a, classified_b) %>%
  group_by(model_name) %>%
  group_modify(~ensure_status_levels(.x)) %>%
  ungroup()

raw_effort_df <- model_a@data %>%
  transmute(TrackID = annotation_id, Species = category_name, video_id)
val_effort_df <- truth@data %>%
  transmute(TrackID = annotation_id, Species = category_name, video_id)
reviewer_effort <- analyze_reviewer_effort(raw_effort_df, val_effort_df, group_vars = "video_id")

# --- 7. Confusion and disagreement reports ---
confusion_df <- calculate_confusion_matrix(
  aligned_a,
  group_vars = c("video_id", "frame_index"),
  species_col = category_name
)

disagreement_report <- get_disagreement_report(
  aligned_a,
  group_vars = c("video_id", "frame_index", "category_name")
)

# analyze_performance_drivers() expects column named Species.
drivers_input <- aligned_a %>% rename(Species = category_name)
try({
  drivers_model <- analyze_performance_drivers(drivers_input, group_vars = "video_id")
  print(summary(drivers_model))
}, silent = TRUE)

print(head(aligned_a_density))
print(head(reviewer_effort))
print(head(disagreement_report))

# --- 8. Plotting helpers and model comparison visuals ---
if (requireNamespace("ggpubr", quietly = TRUE)) {
  p_scatter <- plot_counts_scatterplot(aligned_both, model_col = model_name)
  p_scatter <- p_scatter + theme_optics()
  print(p_scatter)
} else {
  cat("Package 'ggpubr' not installed; skipping scatterplot example.\n")
}

p_bland <- plot_bland_altman(aligned_both, model_col = model_name)
p_perf <- plot_performance_by_threshold(perf_both, model_col = model_name)
p_scalpred_f1 <- plot_scalpred_f1_curve(perf_both, model_col = model_name)
p_scalpred_pr <- plot_scalpred_pr_curve(perf_both, model_col = model_name)
p_confusion_binary <- plot_confusion_matrix(metrics_both, model_col = model_name)
p_confusion_multiclass <- plot_multiclass_confusion_matrix(confusion_df)

print(p_bland)
print(p_perf)
print(p_scalpred_f1)
print(p_scalpred_pr)
print(p_confusion_binary)
print(p_confusion_multiclass)

if (requireNamespace("pROC", quietly = TRUE)) {
  p_roc <- plot_roc_curve(classified_both, model_col = model_name)
  print(p_roc)
} else {
  cat("Package 'pROC' not installed; skipping ROC plot example.\n")
}

if (requireNamespace("PRROC", quietly = TRUE)) {
  p_pr <- plot_pr_curve(classified_both, model_col = model_name)
  print(p_pr)
} else {
  cat("Package 'PRROC' not installed; skipping PR plot example.\n")
}

cat("\nAUV end-to-end example complete.\n")

unlink(model_tmp)
unlink(truth_tmp)
