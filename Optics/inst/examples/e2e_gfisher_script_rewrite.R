# ---
# End-to-End Rewrite: Legacy "Optics Model Performance.R" using Optics package APIs
# ---
# This script reproduces the GFisher MaxN comparison workflow using package
# ingestion/alignment/metric functions instead of manual wrangling code.
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

Allreadtimes <- read.csv(file.path(wrkdir, "All GFISHER Read Times.csv")) %>% 
  dplyr::mutate(StartTime = format(as.POSIXct(StartTime, format = "%H:%M:%S"), "%H:%M:%S"), 
                EndTime = format(as.POSIXct(EndTime, format = "%H:%M:%S"), "%H:%M:%S")) 
Readtimekey <- read.csv(file.path(wrkdir, "final_filled_key_v3.csv")) %>%
  mutate(Videotime = sub("\\..*", "", Timestamp))
Species_List <- read.csv(file.path(wrkdir, "Species List.csv"))
fwri_ref_key <- read.csv(file.path(wrkdir, "env3LABS_93to24.csv")) %>%
  dplyr::mutate(
    SITE_ID = gsub("_|-", "", SITE_ID),
    REFERENCE = gsub("_|-", "", REFERENCE),
    Deployment = ifelse(YEAR < 2024 | LAB == "FWRI", REFERENCE, SITE_ID)
  )

# Locate model and truth inputs from external Data/ folders only.
track_files <- if (dir.exists(trkdir)) {
  list.files(trkdir, pattern = "_tracks.*\\.csv$", full.names = TRUE, recursive = TRUE)
} else {
  character(0)
}

truth_candidates <- c(
  file.path(trthdir, "maxn3LABS_93to24.csv"),
  file.path(wrkdir, "maxn3LABS_93to24.csv")
)
truth_path <- truth_candidates[file.exists(truth_candidates)][1]

if (length(track_files) == 0) {
  stop("No SEFSC track files found in Data/Tracks.")
}
if (is.na(truth_path) || !file.exists(truth_path)) {
  stop("Missing truth file: maxn3LABS_93to24.csv in Data/Truth or Data/.")
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

# Preview conversion of one track CSV to KWCOCO format using the package API.
kwcoco_preview <- convert_track_csv_to_kwcoco(
  track_files[[1]],
  video_name = extract_deployment_id(track_files[[1]])
)
cat("KWCOCO preview annotations:", length(kwcoco_preview$annotations), "\n")

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

##################################################
## Part I: Importing Tracks and Creating Counts ##
##################################################
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

######################################################
## Part II: Groundtruth Alignment and Comparison   ##
######################################################
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

##################################################
## Part III: Data Analysis and Report Creation  ##
##################################################
# 5) Optional summary metric table for this aligned MaxN comparison.
total_comparisons <- nrow(distinct(aligned_maxn, deployment_reference_compact, category_name))
maxn_binary_metrics <- calculate_binary_metrics(
  aligned_maxn %>% rename(video_id = deployment_reference_compact),
  total_comparisons = total_comparisons
)

print(maxn_binary_metrics)

# Preserve legacy report structure by creating/using a script-local analysis Rmd.
analysis_report_path <- file.path(script.dir, "VIAME Output Analysis Report.Rmd")
report_template_candidates <- c(
  file.path(script.dir, "Optics Model Performance Report.Rmd"),
  file.path(getwd(), "Optics Model Performance Report.Rmd")
)
report_template <- report_template_candidates[file.exists(report_template_candidates)][1]

if (!file.exists(analysis_report_path)) {
  if (!is.na(report_template) && file.exists(report_template)) {
    file.copy(report_template, analysis_report_path, overwrite = TRUE)
  } else {
    writeLines(
      c(
        "---",
        "title: \"GFisher Output Analysis Report\"",
        "output: html_document",
        "params:",
        "  spec_master: !r NULL",
        "  sub_false: !r NULL",
        "  species: !r \"\"",
        "  outdir: !r \"\"",
        "---",
        "",
        "# `r if (nzchar(params$species)) params$species else \"GFisher\"` Analysis Report",
        "",
        "# Part I - Counts",
        "",
        "```{r}",
        "if (exists(\"model_maxn\")) print(utils::head(model_maxn))",
        "```",
        "",
        "# Part II - Groundtruthing",
        "",
        "```{r}",
        "if (!is.null(params$spec_master)) {",
        "  print(utils::head(params$spec_master))",
        "} else if (exists(\"model_vs_manual_maxn\")) {",
        "  print(utils::head(model_vs_manual_maxn))",
        "}",
        "```",
        "",
        "# Part III - Data Analysis",
        "",
        "```{r}",
        "if (!is.null(params$sub_false)) {",
        "  print(params$sub_false)",
        "} else if (exists(\"maxn_binary_metrics\")) {",
        "  print(maxn_binary_metrics)",
        "}",
        "```"
      ),
      analysis_report_path
    )
  }
}

# Would you like to cut out large schools (i.e., 999, 299, 399)?
print("Would you like to remove counts with large schools (i.e., 299, 399, and 999)? (y, n)")
remove_large_schools <- rstudioapi::showPrompt(
  title = "Manual Input Required",
  message = "Would you like to remove counts with large schools (i.e., 299, 399, and 999)? Please enter y or n."
)
# checking to see if the user cancelled the value selection
if (is.null(remove_large_schools)) {
  stop("Script cancelled by user.", call. = FALSE)
}
if (file.exists(analysis_report_path)) {
  analysis_reports_dir <- file.path(outdir, "Part III - Data Analysis", "Analysis Reports")
  dir.create(analysis_reports_dir, recursive = TRUE, showWarnings = FALSE)
  species_values <- sort(unique(model_vs_manual_maxn$class_label))
  for (species in species_values) {
    spec_master <- model_vs_manual_maxn %>% filter(class_label == species)
    sub_false <- aligned_maxn %>%
      filter(category_name == species) %>%
      summarise(
        total = n(),
        exact_agreement = sum(model_count == truth_count, na.rm = TRUE),
        agreement_rate = ifelse(total == 0, NA_real_, exact_agreement / total),
        .groups = "drop"
      )
    spec_pretty <- gsub("_", " ", stringr::str_to_sentence(species))
    rmarkdown::render(
      analysis_report_path,
      output_dir = analysis_reports_dir,
      output_format = "html_document",
      output_file = paste(spec_pretty, "Analysis Report.html"),
      params = list(
        spec_master = spec_master,
        sub_false = sub_false,
        species = species,
        outdir = outdir
      ),
      quiet = TRUE
    )
  }
}

cat("\nGFisher package-based rewrite complete.\n")
