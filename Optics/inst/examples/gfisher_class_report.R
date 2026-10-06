# Run after installing Optics and its rmarkdown/knitr suggestions plus Pandoc:
# source(system.file("examples", "gfisher_class_report.R", package = "Optics"))
# result <- run_gfisher_class_report(output_dir = "reports")
# Alternatively: Rscript gfisher_class_report.R reports

run_gfisher_class_report <- function(class_label = NULL,
                                    output_dir = "reports",
                                    confidence_thresholds = c(0.5, 0.9),
                                    remove_large_schools = FALSE) {
  # 1. Locate one original deployment (do not combine its _ST track variant).
  data_dir <- system.file("extdata", "SEFSC", package = "Optics")
  if (!nzchar(data_dir)) stop("Install Optics with its bundled SEFSC data.")
  deployment <- "2024NCO155"
  if (!is.numeric(confidence_thresholds) || !length(confidence_thresholds) ||
      any(!is.finite(confidence_thresholds)) ||
      any(confidence_thresholds < 0 | confidence_thresholds > 1)) {
    stop("confidence_thresholds must contain numbers between zero and one.")
  }
  confidence_thresholds <- unique(confidence_thresholds)
  compact_id <- function(x) gsub("[-_]", "", toupper(trimws(x)))
  species <- utils::read.csv(file.path(data_dir, "Species List.csv"))
  timing <- utils::read.csv(file.path(data_dir, "All GFISHER Read Times.csv"))
  frame_key <- utils::read.csv(file.path(data_dir, "final_filled_key_v3.csv"))
  window <- timing[compact_id(timing$ReferenceID) == deployment, , drop = FALSE]
  if (nrow(window) != 1L) stop("Expected one manual reading window.")
  clock <- function(x) {
    parts <- strsplit(x, ":", fixed = TRUE)
    vapply(parts, function(p) {
      sprintf("%02d:%02d:%02d", as.integer(p[1]), as.integer(p[2]), as.integer(p[3]))
    }, character(1))
  }
  timestamps <- sub("\\..*$", "", frame_key$Timestamp)
  start_frames <- frame_key$Frame[timestamps == clock(window$StartTime)]
  end_frames <- frame_key$Frame[timestamps == clock(window$EndTime)]
  if (!length(start_frames) || !length(end_frames)) stop("Reading window is absent from frame key.")
  frame_bounds <- c(min(start_frames), max(end_frames))

  # 2. Ingest model detections, map species, and retain manually read frames.
  model <- Optics::read_viame_csv(
    file.path(data_dir, "2024-NCO-155_tracks.csv"), video_id = deployment
  )
  raw_labels <- toupper(trimws(model@data$category_name))
  mapped_labels <- toupper(trimws(species$Species[
    match(raw_labels, toupper(trimws(species$Spec_Viame_Dash)))
  ]))
  model@data$category_name <- ifelse(
    is.na(mapped_labels) | !nzchar(mapped_labels), raw_labels, mapped_labels
  )
  model@data <- dplyr::filter(
    model@data, !is.na(category_name), nzchar(category_name),
    frame_index >= frame_bounds[1], frame_index <= frame_bounds[2]
  )
  methods::validObject(model)

  # Manual GFISHER truth is already deployment MaxN, not frame annotations.
  # Discover manual species columns and retain zeros for model-observed classes.
  wide_truth <- utils::read.csv(
    file.path(data_dir, "maxn3LABS_93to24.csv"), check.names = FALSE
  )
  truth_row <- wide_truth[compact_id(wide_truth$REFERENCE) == deployment, , drop = FALSE]
  if (nrow(truth_row) != 1L) stop("Expected one manual MaxN row.")
  truth_columns <- setdiff(names(truth_row), c("LAB", "REFERENCE"))
  truth_counts <- tidyr::pivot_longer(
    truth_row[, truth_columns, drop = FALSE],
    cols = dplyr::everything(), names_to = "category_name", values_to = "truth_count"
  )
  truth_counts$category_name <- toupper(truth_counts$category_name)
  truth_counts$category_name <- trimws(truth_counts$category_name)
  truth_counts <- dplyr::filter(
    truth_counts, !is.na(truth_count),
    truth_count > 0 | category_name %in% model@data$category_name
  )
  truth_counts$video_id <- deployment
  classes <- sort(unique(c(model@data$category_name, truth_counts$category_name)))
  if (!length(classes)) stop("No observed classes in the model or manual data.")
  if (!is.null(class_label) &&
      (!is.character(class_label) || length(class_label) != 1L ||
       is.na(class_label) || !class_label %in% classes)) {
    stop("class_label must be one of: ", paste(classes, collapse = ", "))
  }

  # 3. Calculate MaxN and align count tables at each confidence threshold.
  aligned_runs <- lapply(confidence_thresholds, function(threshold) {
    threshold_model <- model
    threshold_model@data <- dplyr::filter(threshold_model@data, score >= threshold)
    # calculate_maxn groups by score; use one evaluated score per threshold.
    threshold_model@data$score <- rep(threshold, nrow(threshold_model@data))
    model_counts <- Optics::calculate_maxn(threshold_model)
    model_counts$video_id <- as.character(model_counts$video_id)
    model_counts$category_name <- as.character(model_counts$category_name)
    model_counts$score <- as.numeric(model_counts$score)
    model_counts$maxn <- as.numeric(model_counts$maxn)
    aligned <- Optics::align_counts(
      model_counts, truth_counts,
      by = c("video_id", "category_name"),
      model_col = maxn, truth_col = truth_count
    )
    # Keep every observed class even if a threshold removes all its predictions.
    aligned <- dplyr::right_join(
      aligned, data.frame(video_id = deployment, category_name = classes),
      by = c("video_id", "category_name")
    )
    aligned$model_count <- tidyr::replace_na(aligned$model_count, 0)
    aligned$truth_count <- tidyr::replace_na(aligned$truth_count, 0)
    aligned$threshold <- threshold
    aligned
  })
  performance <- Optics::OpticsPerformance(
    dplyr::bind_rows(aligned_runs), grouping_level = "video"
  )

  # 4. Calculate every modular metric, returning the S4 object at each step.
  performance <- Optics::calculate_tp(performance)
  performance <- Optics::calculate_fp(performance)
  performance <- Optics::calculate_fn(performance)
  performance <- Optics::calculate_tn(performance)
  performance <- Optics::calculate_precision(performance)
  performance <- Optics::calculate_recall(performance)
  performance <- Optics::calculate_f1(performance)
  performance <- Optics::calculate_fpr(performance)
  performance <- Optics::calculate_fnr(performance)
  performance <- Optics::calculate_accuracy(performance)
  performance <- Optics::calculate_false_positive_ratio(performance)
  performance <- Optics::calculate_false_negative_ratio(performance)
  performance <- Optics::calculate_total_actual_positives(performance)
  performance <- Optics::calculate_total_actual_negatives(performance)
  performance <- Optics::calculate_agree(performance)
  performance <- Optics::calculate_difference(performance)
  performance <- Optics::calculate_relaxed(performance)

  # 5. Render every included class unless a single class was requested.
  # Remove metric calls above for smaller reports.
  report_classes <- if (is.null(class_label)) {
    sort(unique(performance@data$category_name))
  } else class_label
  filenames <- make.unique(
    gsub("[^a-z0-9_-]", "_", tolower(classes)), sep = "-"
  )
  names(filenames) <- classes
  report_paths <- vapply(report_classes, function(label) {
    path <- Optics::generate_class_report(
      performance, class_label = label,
      output_file = paste0(filenames[[label]], "-performance.html"),
      output_dir = output_dir, grouping_level = "video",
      remove_large_schools = remove_large_schools
    )
    message("Analysis report: ", path)
    path
  }, character(1))
  names(report_paths) <- report_classes
  invisible(list(performance = performance, report_paths = report_paths,
                 report_path = if (length(report_paths) == 1L) unname(report_paths) else NULL,
                 frame_bounds = frame_bounds))
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  run_gfisher_class_report(output_dir = if (length(args)) args[1] else "reports")
}
