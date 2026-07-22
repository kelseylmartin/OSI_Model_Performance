# ---
# End-to-End Rewrite: Legacy "Optics Model Performance.R" using Optics package APIs
# ---
# This script reproduces the GFisher MaxN comparison workflow using package
# ingestion/alignment/metric functions instead of manual wrangling code.

library(Optics)
library(dplyr)
library(purrr)
library(stringr)

# 1) Legacy directory structure setup (lines 247-271) for external execution.
#    This preserves the original Data/Tracks/Truth/Output expectations when
#    users run this script outside package internals.
get_script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  }
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
  }
  normalizePath(getwd())
}

script.dir <- get_script_dir()
data_root_candidates <- c(
  file.path(script.dir, "Data"),
  file.path(script.dir, "..", "..", "..", "Data"),
  file.path(script.dir, "..", "..", "Data"),
  file.path(getwd(), "Data")
)
data_root <- data_root_candidates[dir.exists(data_root_candidates)][1]
if (is.na(data_root)) {
  stop("Could not find a Data directory. Expected one of: ", paste(data_root_candidates, collapse = ", "))
}
wrkdir <- data_root
trkdir <- file.path(data_root, "Tracks")
trthdir <- file.path(data_root, "Truth")

dir.create(file.path(script.dir, "Output"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(script.dir, "Output", "Part I - Counts"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(script.dir, "Output", "Part II - Groundtruthing"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(script.dir, "Output", "Part III - Data Analysis"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(script.dir, "Output", "Part III - Data Analysis", "Figures"), recursive = TRUE, showWarnings = FALSE)
outdir <- file.path(script.dir, "Output")

Allreadtimes <- read.csv(file.path(wrkdir, "All GFISHER Read Times.csv"))
Readtimekey <- read.csv(file.path(wrkdir, "final_filled_key_v2.csv")) %>%
  mutate(Videotime = sub("\\..*", "", Timestamp))
Species_List <- read.csv(file.path(wrkdir, "Species List.csv"))
fwri_ref_key <- read.csv(file.path(wrkdir, "env3LABS_93to24.csv")) %>%
  dplyr::mutate(
    SITE_ID = gsub("_|-", "", SITE_ID),
    REFERENCE = gsub("_|-", "", REFERENCE),
    Deployment = ifelse(YEAR < 2024 | LAB == "FWRI", REFERENCE, SITE_ID)
  )

# Locate model and truth inputs. Prefer external Data/ folders and fall back to
# packaged extdata so the example still runs inside package contexts.
sefsc_dir <- system.file("extdata/SEFSC", package = "Optics")
track_search_dirs <- c(trkdir, sefsc_dir)
track_files <- unique(unlist(lapply(track_search_dirs, function(dir_path) {
  if (!dir.exists(dir_path)) {
    return(character(0))
  }
  list.files(dir_path, pattern = "_tracks.*\\.csv$", full.names = TRUE, recursive = TRUE)
})))

truth_candidates <- c(
  file.path(trthdir, "maxn3LABS_93to24.csv"),
  file.path(wrkdir, "maxn3LABS_93to24.csv"),
  file.path(sefsc_dir, "maxn3LABS_93to24.csv")
)
truth_path <- truth_candidates[file.exists(truth_candidates)][1]

if (length(track_files) == 0) {
  stop("No SEFSC track files found in Data/Tracks or extdata/SEFSC.")
}
if (is.na(truth_path) || !file.exists(truth_path)) {
  stop("Missing truth file: maxn3LABS_93to24.csv in Data/Truth, Data/, or extdata/SEFSC.")
}

extract_deployment_id <- function(path) {
  sub("_tracks.*$", "", basename(path))
}

normalize_reference_id <- function(x) {
  x %>%
    toupper() %>%
    str_replace("^(\\d{4})-(N(?:CD|CO)-\\d{3})$", "\\1_\\2")
}

normalize_compact_id <- function(x) {
  x %>%
    toupper() %>%
    gsub("_|-", "", .)
}

# Legacy frame-window trimming support from the read-time files.
Allreadtimes$ReferenceID <- gsub("_|-", "", Allreadtimes$ReferenceID)
start_lookup <- Allreadtimes %>%
  left_join(Readtimekey, by = c("StartTime" = "Videotime")) %>%
  dplyr::group_by(ReferenceID) %>%
  dplyr::summarise(Start = min(Frame, na.rm = TRUE), .groups = "drop")
end_lookup <- Allreadtimes %>%
  left_join(Readtimekey, by = c("EndTime" = "Videotime")) %>%
  dplyr::group_by(ReferenceID) %>%
  dplyr::summarise(End = max(Frame, na.rm = TRUE), .groups = "drop")
frame_lookup <- full_join(start_lookup, end_lookup, by = "ReferenceID") %>%
  mutate(
    Start = ifelse(is.infinite(Start), NA, Start),
    End = ifelse(is.infinite(End), NA, End)
  )

# 2) Stitch video-level model outputs into one deployment-level S4 object.
#    Legacy script manually merged raw tables; here we use read_viame_csv() so
#    each file is standardized to the Optics schema before binding.
stitched_model_df <- map2_dfr(track_files, extract_deployment_id(track_files), function(track_file, deployment_id) {
  detections <- read_viame_csv(track_file, video_id = deployment_id)

  detections@data %>%
    mutate(
      deployment_id = deployment_id,
      deployment_reference_id = normalize_reference_id(deployment_id),
      deployment_reference_compact = normalize_compact_id(deployment_reference_id),
      source_track_file = basename(track_file)
    )
})

stitched_model_df <- stitched_model_df %>%
  left_join(frame_lookup, by = c("deployment_reference_compact" = "ReferenceID")) %>%
  dplyr::filter((frame_index >= Start & frame_index <= End) | (is.na(Start) & is.na(End))) %>%
  dplyr::select(-any_of(c("Start", "End")))

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
    deployment_reference_compact = normalize_compact_id(deployment_reference_id),
    class_label = category_name,
    true_count = truth_count
  )

fwri_lookup <- fwri_ref_key %>%
  transmute(
    reference_compact = normalize_compact_id(REFERENCE),
    site_compact = normalize_compact_id(SITE_ID)
  )

model_deployments <- unique(stitched_model@data$deployment_reference_compact)
model_lookup <- fwri_lookup %>%
  filter(site_compact %in% model_deployments | reference_compact %in% model_deployments)
truth_refs_to_keep <- unique(c(model_deployments, model_lookup$reference_compact, model_lookup$site_compact))

truth_aligned <- truth_maxn %>%
  filter(deployment_reference_compact %in% truth_refs_to_keep)

cat("Matched truth deployments:", n_distinct(truth_aligned$deployment_reference_compact), "\n")

# 4) Compute model MaxN with calculate_maxn() (replacing manual ftable/summarise).
model_maxn <- calculate_maxn(stitched_model) %>%
  transmute(
    deployment_reference_compact = normalize_compact_id(normalize_reference_id(video_id)),
    class_label_raw = trimws(category_name),
    maxn = maxn
  ) %>%
  left_join(
    Species_List %>%
      transmute(class_label_raw = trimws(Spec_Viame_Dash), class_label = trimws(Species)),
    by = "class_label_raw"
  ) %>%
  mutate(class_label = ifelse(is.na(class_label), class_label_raw, class_label))

truth_aligned <- truth_aligned %>%
  mutate(class_label = trimws(class_label))

model_vs_truth_aligned <- align_counts(
  model_counts = model_maxn %>%
    transmute(video_id = deployment_reference_compact, category_name = class_label, maxn = maxn),
  truth_counts = truth_aligned %>%
    transmute(video_id = deployment_reference_compact, category_name = class_label, true_count = true_count),
  by = c("video_id", "category_name"),
  model_col = maxn,
  truth_col = true_count
)

deployment_label_lookup <- truth_aligned %>%
  distinct(deployment_reference_compact, deployment_reference_id)

aligned_maxn <- model_vs_truth_aligned %>%
  rename(deployment_reference_compact = video_id) %>%
  left_join(deployment_label_lookup, by = "deployment_reference_compact") %>%
  mutate(
    deployment_reference_id = ifelse(
      is.na(deployment_reference_id),
      deployment_reference_compact,
      deployment_reference_id
    )
  )

# Final table mirrors legacy analytical intent: model MaxN vs manual MaxN by
# deployment and species.
model_vs_manual_maxn <- aligned_maxn %>%
  transmute(
    deployment_reference_id = deployment_reference_id,
    class_label = category_name,
    VIAME_MaxN = model_count,
    Manual_MaxN = truth_count,
    Difference = Manual_MaxN - VIAME_MaxN
  ) %>%
  arrange(deployment_reference_id, class_label)

print(model_vs_manual_maxn)

# 6) Optional summary metric table for this aligned MaxN comparison.
total_comparisons <- nrow(distinct(aligned_maxn, deployment_reference_compact, category_name))
maxn_binary_metrics <- calculate_binary_metrics(
  aligned_maxn %>% rename(video_id = deployment_reference_compact),
  total_comparisons = total_comparisons
)

print(maxn_binary_metrics)
cat("\nGFisher package-based rewrite complete.\n")
