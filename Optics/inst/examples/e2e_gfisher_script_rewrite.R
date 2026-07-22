# ---
# End-to-End Rewrite: Legacy "Optics Model Performance.R" using Optics package APIs
# ---
# This script reproduces the GFisher MaxN comparison workflow using package
# ingestion/alignment/metric functions instead of manual wrangling code.

library(Optics)
library(dplyr)
library(purrr)
library(stringr)

# 1) Locate packaged SEFSC inputs (model track files + manual MaxN truth)
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

# 2) Stitch video-level model outputs into one deployment-level S4 object.
#    Legacy script manually merged raw tables; here we use read_viame_csv() so
#    each file is standardized to the Optics schema before binding.
stitched_model_df <- map2_dfr(track_files, extract_deployment_id(track_files), function(track_file, deployment_id) {
  detections <- read_viame_csv(track_file, video_id = deployment_id)

  detections@data %>%
    mutate(
      deployment_id = deployment_id,
      deployment_reference_id = normalize_reference_id(deployment_id),
      source_track_file = basename(track_file)
    )
})

stitched_model <- OpticsDetections(
  stitched_model_df,
  source_file = paste(basename(track_files), collapse = ", "),
  ingest_format = "viame_csv_stitched"
)

cat("Stitched detections:", nrow(stitched_model@data), "\n")

# 3) Read manual MaxN truth with read_wide_maxn() instead of custom pivot code.
#    We then normalize deployment IDs to match model naming before alignment.
truth_maxn <- read_wide_maxn(truth_path, video_id_col = REFERENCE) %>%
  mutate(
    deployment_id = str_extract(video_id, "\\d{4}[_-]N(?:CD|CO)-\\d{3}"),
    deployment_id = ifelse(is.na(deployment_id), video_id, deployment_id),
    deployment_reference_id = normalize_reference_id(deployment_id),
    class_label = category_name,
    true_count = truth_count
  )

model_deployments <- unique(stitched_model@data$deployment_reference_id)
truth_aligned <- truth_maxn %>%
  filter(deployment_reference_id %in% model_deployments)

cat("Matched truth deployments:", n_distinct(truth_aligned$deployment_reference_id), "\n")

# 4) Compute model MaxN with calculate_maxn() (replacing manual ftable/summarise).
model_maxn <- calculate_maxn(stitched_model) %>%
  transmute(
    deployment_reference_id = normalize_reference_id(video_id),
    class_label = category_name,
    maxn = maxn
  )

# 5) Align model MaxN to manual MaxN with align_counts() (replacing manual joins).
aligned_maxn <- align_counts(
  model_counts = model_maxn %>%
    transmute(video_id = deployment_reference_id, category_name = class_label, maxn = maxn),
  truth_counts = truth_aligned %>%
    transmute(video_id = deployment_reference_id, category_name = class_label, true_count = true_count),
  by = c("video_id", "category_name"),
  model_col = maxn,
  truth_col = true_count
)

# Final table mirrors legacy analytical intent: model MaxN vs manual MaxN by
# deployment and species.
model_vs_manual_maxn <- aligned_maxn %>%
  transmute(
    deployment_reference_id = video_id,
    class_label = category_name,
    VIAME_MaxN = model_count,
    Manual_MaxN = truth_count,
    Difference = Manual_MaxN - VIAME_MaxN
  ) %>%
  arrange(deployment_reference_id, class_label)

print(model_vs_manual_maxn)

# 6) Optional summary metric table for this aligned MaxN comparison.
total_comparisons <- nrow(distinct(aligned_maxn, video_id, category_name))
maxn_binary_metrics <- calculate_binary_metrics(
  aligned_maxn,
  total_comparisons = total_comparisons
)

print(maxn_binary_metrics)
cat("\nGFisher package-based rewrite complete.\n")
