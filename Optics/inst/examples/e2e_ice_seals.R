# ---
# End-to-End Example: Ice Seals Dataset
# ---

library(Optics)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)

# --- 1. Discover all model and validated files from extdata ---
extdata_dir <- system.file("extdata/AKFSC", package = "Optics")

model_files <- list.files(extdata_dir, pattern = "_detections\\.csv$", full.names = TRUE)
truth_files <- list.files(extdata_dir, pattern = "_validated\\.csv$", full.names = TRUE)

extract_camera <- function(path) {
  camera <- str_match(basename(path), "_([LCR])_")[, 2]
  ifelse(is.na(camera), "", camera)
}

model_index <- tibble(model_file = model_files, camera = extract_camera(model_files))
truth_index <- tibble(truth_file = truth_files, camera = extract_camera(truth_files))

if (any(model_index$camera == "") || any(truth_index$camera == "")) {
  stop("Could not extract camera position (L/R/C) from one or more file names.")
}
if (anyDuplicated(model_index$camera) || anyDuplicated(truth_index$camera)) {
  stop("Expected one model and one validated file per camera (L/R/C).")
}
if (!setequal(model_index$camera, truth_index$camera)) {
  stop("Model and validated camera sets do not match.")
}

camera_file_map <- inner_join(model_index, truth_index, by = "camera")

# --- 2. Ingest all matched camera files into S4 objects ---
model_detections <- ingest_ice_seals_csv(camera_file_map$model_file)
truth_detections <- ingest_ice_seals_csv(camera_file_map$truth_file)

# Ground-truth camera totals and candidate selection demos.
truth_file_map <- setNames(camera_file_map$truth_file, camera_file_map$camera)
totals <- calculate_ice_seals_totals(
  left_file = truth_file_map[["L"]],
  center_file = truth_file_map[["C"]],
  right_file = truth_file_map[["R"]]
)
print(totals)

candidate_detections <- select_ice_seals_candidates(model_detections)
print(head(candidate_detections))

# --- 3. Subset model detections to images that have validated GT ---
extract_camera_from_image <- function(image_id) str_match(basename(image_id), "_([LCR])_")[, 2]

model_df <- model_detections@data %>% mutate(camera = extract_camera_from_image(image_id))
truth_df <- truth_detections@data %>% mutate(camera = extract_camera_from_image(image_id))

truth_image_index <- truth_df %>% distinct(camera, image_id)
model_a_df <- model_df %>% semi_join(truth_image_index, by = c("camera", "image_id"))
truth_matched_df <- truth_df %>% semi_join(distinct(model_a_df, camera, image_id), by = c("camera", "image_id"))

# Build a second model view for model-comparison examples.
score_cutoff <- stats::quantile(model_a_df$score, probs = 0.75, na.rm = TRUE)
model_b_df <- model_a_df %>% filter(score >= score_cutoff)

model_a <- OpticsDetections(
  data = select(model_a_df, -camera),
  source_file = paste(camera_file_map$model_file, collapse = ", "),
  ingest_format = "viame_csv_ice_seals_model_a_matched"
)
model_b <- OpticsDetections(
  data = select(model_b_df, -camera),
  source_file = paste(camera_file_map$model_file, collapse = ", "),
  ingest_format = "viame_csv_ice_seals_model_b_matched"
)
truth <- OpticsDetections(
  data = select(truth_matched_df, -camera),
  source_file = paste(camera_file_map$truth_file, collapse = ", "),
  ingest_format = "viame_csv_ice_seals_truth_matched"
)

# --- 4. Frame-abundance alignment + model comparison ---
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

# Extract metrics at a specific confidence threshold.
selected_confidence <- 0.8
perf_at_selected_confidence <- perf_both %>%
  filter(threshold == selected_confidence)
print(perf_at_selected_confidence)

# --- 5. Detection overlap analysis (many model detections to one GT target) ---
bbox_iou <- function(ax, ay, aw, ah, bx, by, bw, bh) {
  ax2 <- ax + aw
  ay2 <- ay + ah
  bx2 <- bx + bw
  by2 <- by + bh
  inter_x1 <- pmax(ax, bx)
  inter_y1 <- pmax(ay, by)
  inter_x2 <- pmin(ax2, bx2)
  inter_y2 <- pmin(ay2, by2)
  inter_w <- pmax(0, inter_x2 - inter_x1)
  inter_h <- pmax(0, inter_y2 - inter_y1)
  inter_area <- inter_w * inter_h
  union_area <- (aw * ah) + (bw * bh) - inter_area
  ifelse(union_area <= 0, 0, inter_area / union_area)
}

model_overlap_df <- model_a_df %>%
  select(camera, image_id, frame_index, annotation_id, category_name, bbox_x, bbox_y, bbox_width, bbox_height)
truth_overlap_df <- truth_matched_df %>%
  select(camera, image_id, frame_index, annotation_id, category_name, bbox_x, bbox_y, bbox_width, bbox_height)

overlap_counts <- inner_join(
  model_overlap_df,
  truth_overlap_df,
  by = c("camera", "image_id", "frame_index"),
  suffix = c("_model", "_truth")
) %>%
  mutate(
    iou = bbox_iou(
      bbox_x_model, bbox_y_model, bbox_width_model, bbox_height_model,
      bbox_x_truth, bbox_y_truth, bbox_width_truth, bbox_height_truth
    ),
    same_class = category_name_model == category_name_truth
  ) %>%
  filter(iou >= 0.5, same_class) %>%
  group_by(camera, image_id, frame_index, annotation_id_truth) %>%
  summarise(overlap_model_detections = n_distinct(annotation_id_model), .groups = "drop")

truth_targets <- truth_overlap_df %>%
  distinct(camera, image_id, frame_index, annotation_id_truth = annotation_id)

duplicate_summary <- truth_targets %>%
  left_join(overlap_counts, by = c("camera", "image_id", "frame_index", "annotation_id_truth")) %>%
  mutate(overlap_model_detections = replace_na(overlap_model_detections, 0L)) %>%
  mutate(extra_model_detections = pmax(overlap_model_detections - 1L, 0L)) %>%
  group_by(camera, image_id, frame_index) %>%
  summarise(
    truth_targets = n(),
    extra_model_detections = sum(extra_model_detections),
    .groups = "drop"
  )
print(head(duplicate_summary))

# --- 6. Reviewer correction quantification + classification metrics ---
correction_details <- model_a_df %>%
  select(camera, image_id, frame_index, annotation_id, model_class_label = category_name) %>%
  inner_join(
    truth_matched_df %>%
      select(camera, image_id, frame_index, annotation_id, validated_class_label = category_name),
    by = c("camera", "image_id", "frame_index", "annotation_id")
  ) %>%
  mutate(class_changed = model_class_label != validated_class_label)

correction_summary <- correction_details %>%
  group_by(camera) %>%
  summarise(
    matched_detections = n(),
    changed_classifications = sum(class_changed),
    percent_changed = round(100 * changed_classifications / matched_detections, 2),
    .groups = "drop"
  )
print(correction_summary)

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

# --- 7. Confusion/disagreement reports and visualizations ---
confusion_df <- calculate_confusion_matrix(
  aligned_a,
  group_vars = c("video_id", "frame_index"),
  species_col = category_name
)
disagreement_report <- get_disagreement_report(
  aligned_a,
  group_vars = c("video_id", "frame_index", "category_name")
)
print(head(disagreement_report))

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

cat("\nIce seals end-to-end example complete.\n")
