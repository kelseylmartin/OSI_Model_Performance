# ---
# End-to-End Rewrite: Legacy "Optics Model Performance.R" using Optics package APIs
# ---
required_packages <- c(
  "Optics",
  "dplyr",
  "purrr",
  "stringr",
  "rmarkdown"
)

ensure_example_packages <- function(packages, non_cran_packages = "Optics", repos = "https://cloud.r-project.org") {
  missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  installable_packages <- setdiff(missing_packages, non_cran_packages)

  if (length(installable_packages) > 0) {
    install.packages(installable_packages, repos = repos)
  }

  still_missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(still_missing) > 0) {
    stop(
      "Install missing packages before running this example: ",
      paste(still_missing, collapse = ", ")
    )
  }

  invisible(lapply(packages, library, character.only = TRUE))
}

ensure_example_packages(required_packages)

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

resolve_first_existing <- function(paths, description) {
  existing <- unique(paths[dir.exists(paths) | file.exists(paths)])
  if (length(existing) == 0) {
    stop("Could not find ", description, ". Checked: ", paste(paths, collapse = ", "))
  }
  existing[[1]]
}

read_required_csv <- function(path, required_cols = NULL, label = basename(path)) {
  if (!file.exists(path)) {
    stop("Missing required helper CSV: ", label, " at ", path)
  }

  out <- tryCatch(
    read.csv(path, stringsAsFactors = FALSE),
    error = function(e) {
      stop("Malformed helper CSV '", label, "': ", conditionMessage(e))
    }
  )

  if (!is.null(required_cols) && !all(required_cols %in% names(out))) {
    stop(
      "Malformed helper CSV '",
      label,
      "': missing required columns ",
      paste(setdiff(required_cols, names(out)), collapse = ", ")
    )
  }

  out
}

script.dir <- get_script_dir()
sefsc_extdata <- system.file("extdata/SEFSC", package = "Optics")
data_root_candidates <- c(
  if (nzchar(sefsc_extdata)) sefsc_extdata,
  file.path(getwd(), "inst", "extdata", "SEFSC"),
  file.path(script.dir, "..", "extdata", "SEFSC"),
  file.path(script.dir, "..", "..", "extdata", "SEFSC"),
  file.path(script.dir, "Data"),
  file.path(script.dir, "..", "..", "..", "Data"),
  file.path(script.dir, "..", "..", "Data"),
  file.path(getwd(), "Data")
)
data_root <- resolve_first_existing(data_root_candidates, "SEFSC extdata/Data directory")

using_embedded_sefsc <- identical(basename(normalizePath(data_root)), "SEFSC")

if (using_embedded_sefsc) {
  wrkdir <- data_root
  trkdir <- data_root
  trthdir <- data_root
} else {
  wrkdir <- data_root
  track_dir_candidates <- c(
    file.path(data_root, "Tracks"),
    file.path(data_root, "GFISHER"),
    file.path(data_root, "SEFSC")
  )
  trkdir <- resolve_first_existing(track_dir_candidates, "track directory")
  trthdir <- file.path(data_root, "Truth")
}

outdir <- file.path(script.dir, "Output")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(outdir, "Part I - Counts"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(outdir, "Part II - Groundtruthing"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(outdir, "Part III - Data Analysis"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(outdir, "Part III - Data Analysis", "Figures"), recursive = TRUE, showWarnings = FALSE)

Allreadtimes <- read_required_csv(
  file.path(wrkdir, "All GFISHER Read Times.csv"),
  required_cols = c("ReferenceID", "StartTime", "EndTime"),
  label = "All GFISHER Read Times.csv"
) %>%
  dplyr::mutate(
    StartTime = format(as.POSIXct(StartTime, format = "%H:%M:%S"), "%H:%M:%S"),
    EndTime = format(as.POSIXct(EndTime, format = "%H:%M:%S"), "%H:%M:%S")
  )

Readtimekey <- read_required_csv(
  file.path(wrkdir, "final_filled_key_v3.csv"),
  required_cols = c("Frame", "Timestamp"),
  label = "final_filled_key_v3.csv"
) %>%
  mutate(Videotime = sub("\\..*", "", Timestamp))

Species_List <- read_required_csv(
  file.path(wrkdir, "Species List.csv"),
  required_cols = c("Spec_Viame_Dash", "Species"),
  label = "Species List.csv"
) %>%
  dplyr::select(Spec_Viame_Dash, Species)

fwri_ref_key <- read_required_csv(
  file.path(wrkdir, "env3LABS_93to24.csv"),
  required_cols = c("REFERENCE", "SITE_ID", "LAB", "YEAR"),
  label = "env3LABS_93to24.csv"
) %>%
  dplyr::mutate(
    SITE_ID = gsub("_|-", "", SITE_ID),
    REFERENCE = gsub("_|-", "", REFERENCE),
    Deployment = ifelse(YEAR < 2024 | LAB == "FWRI", REFERENCE, SITE_ID)
  )

track_files <- if (dir.exists(trkdir)) {
  list.files(trkdir, pattern = "_tracks.*\\.csv$", full.names = TRUE, recursive = TRUE)
} else {
  character(0)
}

truth_path <- file.path(trthdir, "maxn3LABS_93to24.csv")

if (length(track_files) == 0) {
  stop("No SEFSC track files found. Looked for *_tracks.csv under: ", trkdir)
}
if (!file.exists(truth_path)) {
  stop("Missing truth file: maxn3LABS_93to24.csv")
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

extract_model_version <- function(track_file, deployment_id = NULL) {
  folder_name <- basename(dirname(track_file))
  expected_prefix <- if (!is.null(deployment_id)) str_extract(deployment_id, "^\\d{4}") else NA_character_
  prefixed_pattern <- if (!is.na(expected_prefix)) paste0("^", expected_prefix, "_(.+)$") else "^\\d{4}_(.+)$"

  if (grepl(prefixed_pattern, folder_name)) {
    return(sub(prefixed_pattern, "\\1", folder_name))
  }

  if (grepl("^\\d{4}_(.+)$", folder_name)) {
    return(sub("^\\d{4}_(.+)$", "\\1", folder_name))
  }

  folder_name
}

kwcoco_preview <- convert_track_csv_to_kwcoco(
  track_files[[1]],
  video_name = extract_deployment_id(track_files[[1]])
)
cat("KWCOCO preview annotations:", length(kwcoco_preview$annotations), "\n")

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

stitched_model_df <- map2_dfr(track_files, extract_deployment_id(track_files), function(track_file, deployment_id) {
  detections <- read_viame_csv(track_file, video_id = deployment_id)

  detections@data %>%
    mutate(
      deployment_id = deployment_id,
      deployment_reference_id = normalize_reference_id(deployment_id),
      deployment_reference_compact = normalize_compact_id(deployment_reference_id),
      source_track_file = basename(track_file),
      year = as.numeric(str_extract(deployment_id, "^\\d{4}")),
      model_version = extract_model_version(track_file, deployment_id)
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

truth_maxn <- read_wide_maxn(truth_path, video_id_col = REFERENCE) %>%
  mutate(
    deployment_id = str_extract(video_id, "\\d{4}[_-]N(?:CD|CO)-\\d{3}"),
    deployment_id = ifelse(is.na(deployment_id), video_id, deployment_id),
    deployment_reference_id = normalize_reference_id(deployment_id),
    deployment_reference_compact = normalize_compact_id(deployment_reference_id),
    class_label = category_name,
    true_count = truth_count,
    year = as.numeric(str_extract(deployment_id, "^\\d{4}"))
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
  filter(deployment_reference_compact %in% truth_refs_to_keep) %>%
  mutate(class_label = trimws(class_label))

deployment_label_lookup <- truth_aligned %>%
  distinct(deployment_reference_compact, deployment_reference_id)

version_by_year <- stitched_model@data %>%
  filter(!is.na(year), !is.na(model_version), model_version != "") %>%
  group_by(year) %>%
  summarise(year_model_version = dplyr::first(model_version), .groups = "drop")

comparison_thresholds <- c(seq(0.1, 0.9, by = 0.1), 0.95)
aligned_threshold_runs <- purrr::map_dfr(comparison_thresholds, function(confidence_threshold) {
  threshold_model_df <- stitched_model@data %>%
    filter(score > confidence_threshold) %>%
    mutate(score = confidence_threshold)

  threshold_version_lookup <- threshold_model_df %>%
    distinct(deployment_reference_compact, model_version)

  threshold_maxn <- if (nrow(threshold_model_df) == 0) {
    dplyr::tibble(
      deployment_reference_compact = character(),
      class_label = character(),
      maxn = numeric(),
      year = numeric(),
      model_version = character()
    )
  } else {
    calculate_maxn(threshold_model_df) %>%
      transmute(
        deployment_reference_compact = normalize_compact_id(normalize_reference_id(video_id)),
        class_label_raw = trimws(category_name),
        maxn = maxn,
        year = as.numeric(str_extract(video_id, "^\\d{4}"))
      ) %>%
      left_join(threshold_version_lookup, by = "deployment_reference_compact") %>%
      left_join(
        Species_List %>%
          transmute(class_label_raw = trimws(Spec_Viame_Dash), class_label = trimws(Species)),
        by = "class_label_raw"
      ) %>%
      mutate(class_label = ifelse(is.na(class_label) | class_label == "", class_label_raw, class_label)) %>%
      select(deployment_reference_compact, class_label, maxn, year, model_version)
  }

  aligned_threshold <- align_counts(
    model_counts = threshold_maxn %>%
      transmute(
        video_id = deployment_reference_compact,
        category_name = class_label,
        maxn = maxn,
        year = year,
        model_version = model_version
      ),
    truth_counts = truth_aligned %>%
      transmute(
        video_id = deployment_reference_compact,
        category_name = class_label,
        true_count = true_count,
        year = year
      ),
    by = c("video_id", "category_name"),
    model_col = maxn,
    truth_col = true_count
  )

  aligned_threshold %>%
    rename(deployment_reference_compact = video_id) %>%
    left_join(deployment_label_lookup, by = "deployment_reference_compact") %>%
    mutate(
      deployment_reference_id = ifelse(
        is.na(deployment_reference_id),
        deployment_reference_compact,
        deployment_reference_id
      ),
      year = dplyr::coalesce(year.x, year.y, as.numeric(str_extract(deployment_reference_id, "^\\d{4}")))
    ) %>%
    left_join(version_by_year, by = "year") %>%
    mutate(
      Version = dplyr::coalesce(model_version, year_model_version, "unknown"),
      Confidence = confidence_threshold,
      Species = trimws(category_name),
      Deployment = deployment_reference_id,
      VIAME_MaxN = model_count,
      Manual = truth_count,
      Difference = Manual - VIAME_MaxN,
      Model_info = paste(year, Version, Confidence, sep = "_")
    ) %>%
    select(Deployment, year, Version, Confidence, Species, VIAME_MaxN, Manual, Difference, Model_info)
})

combined_master <- aligned_threshold_runs %>%
  distinct(Deployment, Species, Confidence, .keep_all = TRUE) %>%
  filter(!is.na(year))

if (nrow(combined_master) == 0) {
  stop("No deployment-level MaxN comparisons were generated from the stitched model tracks and REFERENCE data.")
}

metrics <- calculate_legacy_metrics(combined_master, species = "all")
percent_agreement <- calculate_percent_metric(metrics, year, Species, Confidence, Agree)
relaxed_agreement <- calculate_percent_metric(metrics, year, Species, Confidence, Relaxed)

confidence_summary <- percent_agreement %>%
  group_by(Confidence) %>%
  summarise(mean_percent_agreement = mean(percentage_1s, na.rm = TRUE), .groups = "drop")

optimal_confidence <- confidence_summary %>%
  slice_max(order_by = mean_percent_agreement, n = 1, with_ties = FALSE) %>%
  pull(Confidence)

cat("Optimal confidence threshold:", if (length(optimal_confidence) == 0) NA else optimal_confidence, "\n")

analysis_report_path <- file.path(script.dir, "e2e_gfisher_script_markdown.Rmd")
if (file.exists(analysis_report_path)) {
  analysis_reports_dir <- file.path(outdir, "Part III - Data Analysis", "Analysis Reports")
  dir.create(analysis_reports_dir, recursive = TRUE, showWarnings = FALSE)
  rmarkdown::render(
    analysis_report_path,
    output_dir = analysis_reports_dir,
    output_format = "html_document",
    quiet = TRUE,
    params = list(
      species = NULL,
      outdir = outdir,
      remove_large_schools = "n"
    )
  )
}

cat("\nGFisher package-based rewrite complete.\n")
