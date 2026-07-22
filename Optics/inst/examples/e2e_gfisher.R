# ---
# End-to-End Example: GFisher Stationary Benthic Video Dataset
# ---

library(Optics)
library(dplyr)
library(purrr)
library(stringr)

# --- 1. Locate extdata sources ---
sefsc_dir <- system.file("extdata/SEFSC", package = "Optics")
truth_path <- file.path(sefsc_dir, "maxn3LABS_93to24.csv")

track_files <- list.files(sefsc_dir, pattern = "_tracks.*\\.csv$", full.names = TRUE)

if (length(track_files) == 0) {
  stop("No SEFSC track files found in extdata/SEFSC.")
}
if (!file.exists(truth_path)) {
  stop("Missing truth file: ", truth_path)
}

extract_deployment_id <- function(path) {
  sub("_tracks.*$", "", basename(path))
}

normalize_reference_id <- function(x) {
  x %>%
    toupper() %>%
    str_replace("^(\\d{4})-(N(?:CD|CO)-\\d{3})$", "\\1_\\2")
}

# --- 2. Stitch all video-level model outputs into one S4 object ---
stitched_model_df <- map2_dfr(track_files, extract_deployment_id(track_files), function(track_file, deployment_id) {
  detections <- read_viame_csv(track_file, video_id = deployment_id)
  detections@data %>%
    mutate(
      deployment_id = deployment_id,
      deployment_reference_id = normalize_reference_id(deployment_id),
      video_id = deployment_id,
      source_track_file = basename(track_file)
    )
})

# Build a second model variant for model-comparison examples.
score_cutoff <- stats::quantile(stitched_model_df$score, probs = 0.75, na.rm = TRUE)
model_a_df <- stitched_model_df
model_b_df <- stitched_model_df %>% filter(score >= score_cutoff)

model_a <- OpticsDetections(model_a_df, paste(basename(track_files), collapse = ", "), "viame_csv_stitched_model_a")
model_b <- OpticsDetections(model_b_df, paste(basename(track_files), collapse = ", "), "viame_csv_stitched_model_b")

cat("Stitched model A detections:", nrow(model_a@data), "\n")
cat("Stitched model B detections:", nrow(model_b@data), "\n")

# --- 3. Read GT wide MaxN and align deployment identifiers ---
truth_maxn <- read_wide_maxn(truth_path, video_id_col = REFERENCE) %>%
  mutate(
    deployment_id = str_extract(video_id, "\\d{4}[_-]N(?:CD|CO)-\\d{3}"),
    deployment_id = ifelse(is.na(deployment_id), video_id, deployment_id),
    deployment_reference_id = normalize_reference_id(deployment_id),
    class_label = category_name,
    true_count = truth_count
  )

model_deployments <- unique(model_a@data$deployment_reference_id)
truth_aligned <- truth_maxn %>%
  filter(deployment_reference_id %in% model_deployments)

cat("Matched truth deployments:", n_distinct(truth_aligned$deployment_reference_id), "\n")

# --- 4. Calculate MaxN for both models and compare to manual truth ---
model_a_maxn <- calculate_maxn(model_a) %>%
  transmute(
    deployment_reference_id = normalize_reference_id(video_id),
    class_label = category_name,
    maxn = maxn
  )

model_b_maxn <- calculate_maxn(model_b) %>%
  transmute(
    deployment_reference_id = normalize_reference_id(video_id),
    class_label = category_name,
    maxn = maxn
  )

aligned_a <- align_counts(
  model_counts = model_a_maxn %>%
    transmute(video_id = deployment_reference_id, category_name = class_label, maxn = maxn),
  truth_counts = truth_aligned %>%
    transmute(video_id = deployment_reference_id, category_name = class_label, true_count = true_count),
  by = c("video_id", "category_name"),
  model_col = maxn,
  truth_col = true_count
) %>% mutate(model_name = "model_a")

aligned_b <- align_counts(
  model_counts = model_b_maxn %>%
    transmute(video_id = deployment_reference_id, category_name = class_label, maxn = maxn),
  truth_counts = truth_aligned %>%
    transmute(video_id = deployment_reference_id, category_name = class_label, true_count = true_count),
  by = c("video_id", "category_name"),
  model_col = maxn,
  truth_col = true_count
) %>% mutate(model_name = "model_b")

aligned_both <- bind_rows(aligned_a, aligned_b) %>%
  rename(deployment_reference_id = video_id, class_label = category_name, true_count = truth_count) %>%
  mutate(maxn_difference = model_count - true_count)

all_groups <- bind_rows(
  distinct(aligned_a, video_id, category_name),
  distinct(aligned_b, video_id, category_name)
)
total_comparisons <- nrow(distinct(all_groups))

metrics_a <- calculate_binary_metrics(aligned_a, total_comparisons = total_comparisons)
metrics_b <- calculate_binary_metrics(aligned_b, total_comparisons = total_comparisons)
metrics_both <- bind_rows(
  mutate(metrics_a, model_name = "model_a"),
  mutate(metrics_b, model_name = "model_b")
)

print(metrics_both)

# Additional utility demos relevant to MaxN-aligned data.
aligned_a_density <- calculate_density(aligned_a, count_col = model_count, area = 1)
print(head(aligned_a_density))

confusion_df <- calculate_confusion_matrix(
  aligned_a,
  group_vars = c("video_id"),
  species_col = category_name
)
disagreement_report <- get_disagreement_report(
  aligned_a,
  group_vars = c("video_id", "category_name")
)
print(head(disagreement_report))

# analyze_performance_drivers() expects a Species column.
drivers_input <- aligned_a %>% rename(Species = category_name)
try({
  drivers_model <- analyze_performance_drivers(drivers_input, group_vars = "video_id")
  print(summary(drivers_model))
}, silent = TRUE)

# --- 5. Plot model-comparison outputs ---
if (requireNamespace("ggpubr", quietly = TRUE)) {
  p_scatter <- plot_counts_scatterplot(aligned_both, model_col = model_name)
  p_scatter <- p_scatter + theme_optics()
  print(p_scatter)
} else {
  cat("Package 'ggpubr' not installed; skipping scatterplot example.\n")
}

p_bland <- plot_bland_altman(aligned_both, model_col = model_name)
p_confusion_binary <- plot_confusion_matrix(metrics_both, model_col = model_name)
p_confusion_multiclass <- plot_multiclass_confusion_matrix(confusion_df)

print(p_bland)
print(p_confusion_binary)
print(p_confusion_multiclass)

cat("\nGFisher end-to-end example complete.\n")
cat("Note: detection-level ROC/PR and reviewer-effort examples are not shown here because GFisher ground truth is deployment-level MaxN, not per-detection labels.\n")
