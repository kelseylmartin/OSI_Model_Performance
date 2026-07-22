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

track_files <- list.files(
  sefsc_dir,
  pattern = "_tracks.*\\.csv$",
  full.names = TRUE
)

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

# --- 2. Stitch all video-level model outputs into one OpticsDetections object ---
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

model_stitched <- OpticsDetections(
  data = stitched_model_df,
  source_file = paste(basename(track_files), collapse = ", "),
  ingest_format = "viame_csv_stitched_tracks"
)

cat("Stitched model detections:", nrow(model_stitched@data), "rows across", length(unique(model_stitched@data$deployment_id)), "deployments.\n")

# --- 3. Read GT wide MaxN and align deployment identifiers ---
truth_maxn <- read_wide_maxn(truth_path, video_id_col = REFERENCE) %>%
  mutate(
    deployment_id = str_extract(video_id, "\\d{4}[_-]N(?:CD|CO)-\\d{3}"),
    deployment_id = ifelse(is.na(deployment_id), video_id, deployment_id),
    deployment_reference_id = normalize_reference_id(deployment_id),
    class_label = category_name
  )

model_deployments <- unique(model_stitched@data$deployment_reference_id)
truth_aligned <- truth_maxn %>%
  filter(deployment_reference_id %in% model_deployments)

# --- 4. Calculate model MaxN and compare against manual GT MaxN ---
model_maxn <- calculate_maxn(model_stitched) %>%
  transmute(
    deployment_id = video_id,
    deployment_reference_id = normalize_reference_id(video_id),
    class_label = category_name,
    model_maxn = maxn
  )

aligned_maxn <- align_counts(
  model_counts = model_maxn %>%
    transmute(video_id = deployment_reference_id, category_name = class_label, maxn = model_maxn),
  truth_counts = truth_aligned %>%
    transmute(video_id = deployment_reference_id, category_name = class_label, true_count = truth_count),
  by = c("video_id", "category_name"),
  model_col = maxn,
  truth_col = true_count
) %>%
  rename(deployment_reference_id = video_id, class_label = category_name, true_count = truth_count) %>%
  mutate(maxn_difference = model_count - true_count)

cat("Aligned MaxN rows:", nrow(aligned_maxn), "\n")
cat("Matched truth deployments:", n_distinct(truth_aligned$deployment_reference_id), "\n")
cat("\nSample MaxN comparison:\n")
print(head(aligned_maxn))

cat("\nGFisher end-to-end example complete.\n")
