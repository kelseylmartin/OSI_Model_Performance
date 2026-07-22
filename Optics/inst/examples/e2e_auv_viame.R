# ---
# End-to-End Example: AUV VIAME Test Dataset
# ---

# This script demonstrates a full Optics workflow using AUV extdata files:
# - model output: file name containing "detections"
# - ground truth: file name containing "groundtruth"
#
# For model-comparison examples, this script assumes two detections-like model
# input files.

library(Optics)
library(dplyr)
library(tidyr)

# --- 1. Define your data folder and file paths ---
# Update this path to the folder where your AUV files are stored.
data_dir <- "C:/Users/Kelsey.l.martin.NMFS/Documents/NMFS/Automation/OSI_Model_Performance/Optics/inst/extdata"

# Required files:
# - one ground truth file (manually corrected)
# - two model output files for model-comparison examples
truth_path <- file.path(data_dir, "AUV_viame_test_groundtruth.csv")
model_path_a <- file.path(data_dir, "AUV_viame_test_detections.csv")
model_path_b <- file.path(data_dir, "AUV_viame_test_detections.csv")

required_files <- c(model_path_a, model_path_b, truth_path)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing required files:\n", paste(missing_files, collapse = "\n"))
}

cat("Model A file:", model_path_a, "\n")
cat("Model B file:", model_path_b, "\n")
cat("Ground truth file:", truth_path, "\n\n")

# --- 2. Ingest KWCOCO data (read_kwcoco + OpticsDetections class) ---
model_a_csv <- read.csv(model_path_a, header = F)
truth <- read.csv(truth_path, header = F)
model_b_csv <- read.csv(model_path_b, header = F)


# Redifing models as s4 objects
model_a <- setClass(
  "ViameModela",                  # The name of your new class
  slots = list(data = "data.frame"))
model_a <-  new("ViameModela", data = model_a_csv)

model_b <- setClass(
  "ViameModelb",                  # The name of your new class
  slots = list(data = "data.frame"))
model_b <-  new("ViameModelb", data = model_b_csv)

# --- 3. Demonstrate read_viame_csv() using AUV-derived rows ---
# Build a temporary VIAME CSV from AUV model A detections.
viame_tmp <- tempfile(fileext = ".csv")
viame_df <- model_a@data %>%
  transmute(
    TrackID = as.character(V1),
    VidIdent = as.character(V2),
    UniqFrame = as.character(V3),
    TL_X = as.character(V4),
    TL_Y = as.character(V5),
    BR_X = as.character(V6),
    BR_Y = as.character(V7),
    DetLen_Conf = as.character(V8),
    Tar_Len = "1",
    SP = as.character(V10),
    CP = "1"
  )

writeLines(
  c("# 1: Detection or Track-id", "# 2: Video or Image Identifier"),
  con = viame_tmp
)
write.table(viame_df, file = viame_tmp, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)

model_a_viame <- read_viame_csv(viame_tmp, video_id = "AUV_demo")
cat("Rows from read_viame_csv() demo:", nrow(model_a_viame@data), "\n\n")

# --- 4. Core counting functions ---
model_a_frame_abundance <- calculate_frame_abundance(model_a)
model_b_frame_abundance <- calculate_frame_abundance(model_b)
truth_frame_abundance <- calculate_frame_abundance(truth)

cat("Frame abundance rows (model A):", nrow(model_a_frame_abundance), "\n\n")

# --- 5. Demonstrate read_wide_maxn() using AUV-derived truth counts ---
truth_wide_tmp <- tempfile(fileext = ".csv")
truth_wide <- truth_frame_abundance %>%
  group_by(video_id, category_name) %>%
  summarise(truth_count = sum(abundance), .groups = "drop") %>%
  pivot_wider(names_from = category_name, values_from = truth_count, values_fill = 0) %>%
  rename(REFERENCE = video_id)

write.csv(truth_wide, truth_wide_tmp, row.names = FALSE)
truth_from_wide <- read_wide_maxn(truth_wide_tmp, video_id_col = REFERENCE)
cat("Rows from read_wide_maxn() demo:", nrow(truth_from_wide), "\n\n")

# --- 6. Align counts and compute metrics ---
aligned_a <- align_counts(
  model_a_frame_abundance,
  truth_frame_abundance,
  by = c("video_id", "frame_index", "category_name"),
  model_col = abundance,
  truth_col = abundance
)
aligned_b <- align_counts(
  model_b_frame_abundance,
  truth_frame_abundance,
  by = c("video_id", "frame_index", "category_name"),
  model_col = abundance,
  truth_col = abundance
)

aligned_a <- aligned_a %>% mutate(model_name = "model_a")
aligned_b <- aligned_b %>% mutate(model_name = "model_b")
aligned_both <- bind_rows(aligned_a, aligned_b)

# calculate_density() demo
aligned_a_density <- calculate_density(aligned_a, count_col = model_count, area = 1)

# calculate_binary_metrics() demo (single and model-comparison)
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

# --- 7. Threshold performance summary + plot ---
perf_a <- summarize_performance_by_threshold(
  model_detections = model_a,
  truth_detections = truth,
  by = c("video_id", "category_name")
) %>% mutate(model_name = "model_a")

perf_b <- summarize_performance_by_threshold(
  model_detections = model_b,
  truth_detections = truth,
  by = c("video_id", "category_name")
) %>% mutate(model_name = "model_b")

perf_both <- bind_rows(perf_a, perf_b)

# --- 8. Detection-level classification and reviewer effort ---
classified_a <- classify_detections(
  raw_detections = model_a@data,
  validated_detections = truth@data,
  detection_id = annotation_id
) %>%
  mutate(model_name = "model_a")

classified_b <- classify_detections(
  raw_detections = model_b@data,
  validated_detections = truth@data,
  detection_id = annotation_id
) %>%
  mutate(model_name = "model_b")

classified_both <- bind_rows(classified_a, classified_b)

# Ensure each model has both TP and FP statuses for ROC/PR plotting examples.
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

classified_both <- classified_both %>%
  group_by(model_name) %>%
  group_modify(~ensure_status_levels(.x)) %>%
  ungroup()

# analyze_reviewer_effort() expects TrackID/Species columns
raw_effort_df <- model_a@data %>%
  transmute(TrackID = annotation_id, Species = category_name, video_id)
val_effort_df <- truth@data %>%
  transmute(TrackID = annotation_id, Species = category_name, video_id)

reviewer_effort <- analyze_reviewer_effort(raw_effort_df, val_effort_df, group_vars = "video_id")
print(reviewer_effort)

# --- 9. Confusion and disagreement outputs ---
confusion_df <- calculate_confusion_matrix(
  aligned_a,
  group_vars = c("video_id", "frame_index"),
  species_col = category_name
)

disagreement_report <- get_disagreement_report(
  aligned_a,
  group_vars = c("video_id", "frame_index", "category_name")
)

# analyze_performance_drivers() expects column named Species
drivers_input <- aligned_a %>% rename(Species = category_name)
# Note: this can fail for tiny datasets or sparse categories.
try({
  drivers_model <- analyze_performance_drivers(drivers_input, group_vars = "video_id")
  print(summary(drivers_model))
}, silent = TRUE)

# --- 10. Plotting functions ---
p_scatter <- plot_counts_scatterplot(aligned_both, model_col = model_name)
p_bland <- plot_bland_altman(aligned_both, model_col = model_name)
p_perf <- plot_performance_by_threshold(perf_both, model_col = model_name)

# Confusion-matrix tile plot needs tp/fp/fn/tn
p_confusion_binary <- plot_confusion_matrix(metrics_both, model_col = model_name)
p_confusion_multiclass <- plot_multiclass_confusion_matrix(confusion_df)

# ROC / PR from classified detections
p_roc <- plot_roc_curve(classified_both, model_col = model_name)
p_pr <- plot_pr_curve(classified_both, model_col = model_name)

# Theme helper demo
p_scatter <- p_scatter + theme_optics()

# Print plots in interactive sessions
print(p_scatter)
print(p_bland)
print(p_perf)
print(p_confusion_binary)
print(p_confusion_multiclass)
print(p_roc)
print(p_pr)

cat("\nAUV end-to-end example complete.\n")

# Clean up temp files
unlink(viame_tmp)
unlink(truth_wide_tmp)
