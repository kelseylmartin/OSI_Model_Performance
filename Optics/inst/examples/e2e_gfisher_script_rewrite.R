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

truth_candidates <- c(
  file.path(trthdir, "maxn3LABS_93to24.csv"),
  file.path(wrkdir, "maxn3LABS_93to24.csv"),
  file.path(trkdir, "maxn3LABS_93to24.csv")
)
truth_path <- truth_candidates[file.exists(truth_candidates)][1]

if (length(track_files) == 0) {
  stop("No SEFSC track files found. Looked for *_tracks.csv under: ", trkdir)
}
if (is.na(truth_path)) {
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
      year = as.numeric(str_extract(deployment_id, "^\\d{4}"))
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

comparison_thresholds <- c(seq(0.1, 0.9, by = 0.1), 0.95)
aligned_threshold_runs <- purrr::map_dfr(comparison_thresholds, function(confidence_threshold) {
  threshold_model_df <- stitched_model@data %>%
    filter(score > confidence_threshold) %>%
    mutate(score = confidence_threshold)

  threshold_maxn <- if (nrow(threshold_model_df) == 0) {
    dplyr::tibble(
      deployment_reference_compact = character(),
      class_label = character(),
      maxn = numeric(),
      year = numeric()
    )
  } else {
    calculate_maxn(threshold_model_df) %>%
      transmute(
        deployment_reference_compact = normalize_compact_id(normalize_reference_id(video_id)),
        class_label_raw = trimws(category_name),
        maxn = maxn,
        year = as.numeric(str_extract(video_id, "^\\d{4}"))
      ) %>%
      left_join(
        Species_List %>%
          transmute(class_label_raw = trimws(Spec_Viame_Dash), class_label = trimws(Species)),
        by = "class_label_raw"
      ) %>%
      mutate(class_label = ifelse(is.na(class_label) | class_label == "", class_label_raw, class_label)) %>%
      select(deployment_reference_compact, class_label, maxn, year)
  }

  aligned_threshold <- align_counts(
    model_counts = threshold_maxn %>%
      transmute(
        video_id = deployment_reference_compact,
        category_name = class_label,
        maxn = maxn,
        year = year
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
      year = dplyr::coalesce(year.x, year.y, as.numeric(str_extract(deployment_reference_id, "^\\d{4}"))),
      Version = "Optics package pipeline",
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

calculate_metrics <- function(df, species = "none") {
  summarize_metrics <- function(.data) {
    .data %>% dplyr::summarise(
      Agree = ifelse(Manual == VIAME_MaxN, 1, 0),
      Difference = Manual - VIAME_MaxN,
      Relaxed = ifelse(Difference == 1 | Difference == -1 | Difference == 0, 1, 0),
      TP = sum(Manual > 0 & VIAME_MaxN > 0, na.rm = TRUE),
      FP = sum(Manual == 0 & VIAME_MaxN > 0, na.rm = TRUE),
      FN = sum(Manual > 0 & VIAME_MaxN == 0, na.rm = TRUE),
      TN = sum(Manual == 0 & VIAME_MaxN == 0, na.rm = TRUE),
      Precision = TP / (TP + FP),
      Recall_TPR = TP / (TP + FN),
      FPR = FP / (FP + TN),
      FNR = FN / (TP + FN),
      Accuracy = (TP + TN) / (TP + FP + FN + TN),
      False_P_Ratio = FP / (TP + FN + TN),
      False_N_Ratio = FN / (TP + FP + TN),
      Total_Actual_Positives = TP + FN,
      Total_Actual_Negatives = FP + TN,
      .groups = "drop"
    )
  }

  if (species == "none") {
    df_out <- df %>%
      dplyr::group_by(year, Version, Confidence) %>%
      summarize_metrics()
  } else if (species == "all") {
    df_out <- df %>%
      dplyr::group_by(year, Version, Confidence, Species) %>%
      summarize_metrics()
  } else if (any(species %in% df$Species) == TRUE) {
    df_out <- df %>%
      dplyr::filter(Species == species) %>%
      dplyr::group_by(year, Version, Confidence, Species) %>%
      summarize_metrics()
  } else {
    print("No species detected with that name. Check spelling and try again.")
    return(NULL)
  }

  df_out %>%
    dplyr::mutate(across(where(is.numeric), ~ ifelse(is.nan(.), NA, .))) %>%
    dplyr::mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), NA, .)))
}

percent_metric <- function(df, variable1, variable2, group, metric) {
  df %>%
    group_by({{ variable1 }}, {{ variable2 }}, {{ group }}) %>%
    summarise(
      percentage_1s = mean({{ metric }}) * 100,
      percentage_0s = (1 - mean({{ metric }})) * 100,
      count = n(),
      .groups = "drop"
    )
}

metrics <- calculate_metrics(combined_master, species = "all")
percent_agreement <- percent_metric(metrics, year, Species, Confidence, Agree)
relaxed_agreement <- percent_metric(metrics, year, Species, Confidence, Relaxed)

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
