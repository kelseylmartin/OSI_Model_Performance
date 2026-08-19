#' Launch the Optics Shiny Portal
#'
#' Starts the packaged Optics Shiny application for exploring bundled example
#' datasets.
#'
#' @param ... Additional arguments passed to [shiny::runApp()].
#' @return The result of [shiny::runApp()].
#' @export
run_optics_app <- function(...) {
  required_packages <- c("shiny", "bslib", "plotly", "DT")
  missing_packages <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing_packages) > 0) {
    stop(
      "The Optics Shiny portal requires these packages to be installed: ",
      paste(missing_packages, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  app_dir <- .optics_portal_app_dir()
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    stop("Could not locate the packaged Optics Shiny application.", call. = FALSE)
  }

  shiny::runApp(appDir = app_dir, ...)
}

.optics_portal_app_dir <- function() {
  system.file("shiny", "optics_portal", package = "Optics")
}

.normalize_optics_portal_reference_id <- function(x) {
  x <- toupper(as.character(x))
  sub("^([0-9]{4})-(N(?:CD|CO)-[0-9]{3})$", "\\1_\\2", x, perl = TRUE)
}

.normalize_optics_portal_compact_id <- function(x) {
  gsub("_|-", "", .normalize_optics_portal_reference_id(x))
}

.optics_portal_thresholds <- function(selected_threshold = NULL) {
  sort(unique(c(seq(0.1, 0.9, by = 0.1), 0.95, selected_threshold)))
}

.discover_optics_portal_examples <- function(extdata_dir = system.file("extdata", package = "Optics")) {
  examples <- list()

  auv_model <- file.path(extdata_dir, "NWFSC", "AUV_viame_test_detections.csv")
  auv_truth <- file.path(extdata_dir, "NWFSC", "AUV_viame_test_groundtruth.csv")
  if (file.exists(auv_model) && file.exists(auv_truth)) {
    examples[[length(examples) + 1]] <- dplyr::tibble(
      key = "auv_frame_abundance",
      label = "AUV Frame Abundance",
      default_count_metric = "Frame Abundance"
    )
  }

  ice_dir <- file.path(extdata_dir, "AKFSC")
  ice_model_files <- list.files(ice_dir, pattern = "_ir_detections\\.csv$", full.names = TRUE)
  ice_truth_files <- list.files(ice_dir, pattern = "_validated\\.csv$", full.names = TRUE)
  if (length(ice_model_files) > 0 && length(ice_truth_files) > 0) {
    examples[[length(examples) + 1]] <- dplyr::tibble(
      key = "aerial_ice_seals",
      label = "Aerial Ice Seals",
      default_count_metric = "Frame Abundance"
    )
  }

  benthic_truth <- file.path(extdata_dir, "SEFSC", "maxn3LABS_93to24.csv")
  benthic_tracks <- list.files(file.path(extdata_dir, "SEFSC"), pattern = "_tracks.*\\.csv$", full.names = TRUE)
  if (file.exists(benthic_truth) && length(benthic_tracks) > 0) {
    examples[[length(examples) + 1]] <- dplyr::tibble(
      key = "stationary_benthic_maxn",
      label = "Stationary Benthic MaxN",
      default_count_metric = "MaxN"
    )
  }

  if (length(examples) == 0) {
    return(dplyr::tibble(
      key = character(),
      label = character(),
      default_count_metric = character()
    ))
  }

  dplyr::bind_rows(examples)
}

.optics_portal_add_viame_headers <- function(file_path) {
  temp_path <- tempfile(fileext = ".csv")
  writeLines(
    c(
      "# 1: Detection or Track-id, 2: Video or Image Identifier",
      "# 2: Synthetic header inserted for read_viame_csv()"
    ),
    con = temp_path
  )
  cat(readLines(file_path, warn = FALSE), file = temp_path, sep = "\n", append = TRUE)
  temp_path
}

.optics_portal_empty_detection_data <- function() {
  dplyr::tibble(
    video_id = character(),
    image_id = character(),
    frame_index = numeric(),
    annotation_id = character(),
    category_name = character(),
    bbox_x = numeric(),
    bbox_y = numeric(),
    bbox_width = numeric(),
    bbox_height = numeric(),
    score = numeric()
  )
}

.optics_portal_truth_counts_to_detections <- function(truth_counts,
                                                      source_file = "uploaded_truth.csv",
                                                      ingest_format = "uploaded_truth_counts") {
  required_cols <- c("video_id", "category_name", "truth_count")
  missing_cols <- setdiff(required_cols, names(truth_counts))
  if (length(missing_cols) > 0) {
    stop(
      "Truth count data is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  truth_counts <- as.data.frame(truth_counts, stringsAsFactors = FALSE)
  truth_counts$truth_count <- suppressWarnings(as.integer(as.numeric(truth_counts$truth_count)))

  if (any(is.na(truth_counts$truth_count)) || any(truth_counts$truth_count < 0)) {
    stop("`true_count`/`truth_count` must contain non-negative whole numbers.", call. = FALSE)
  }

  truth_counts <- truth_counts[truth_counts$truth_count > 0, , drop = FALSE]
  if (nrow(truth_counts) == 0) {
    return(OpticsDetections(.optics_portal_empty_detection_data(), source_file, ingest_format))
  }

  expanded <- truth_counts[rep(seq_len(nrow(truth_counts)), truth_counts$truth_count), , drop = FALSE]
  expanded$row_id <- ave(seq_len(nrow(expanded)), expanded$video_id, expanded$category_name, FUN = seq_along)

  image_ids <- if ("image_id" %in% names(expanded)) {
    as.character(expanded$image_id)
  } else {
    paste0(expanded$video_id, "_truth_", expanded$row_id)
  }

  detection_data <- dplyr::tibble(
    video_id = as.character(expanded$video_id),
    image_id = image_ids,
    frame_index = if ("frame_index" %in% names(expanded)) {
      suppressWarnings(as.numeric(expanded$frame_index))
    } else {
      rep(1, nrow(expanded))
    },
    annotation_id = paste0("truth_", seq_len(nrow(expanded))),
    category_name = as.character(expanded$category_name),
    bbox_x = rep(0, nrow(expanded)),
    bbox_y = rep(0, nrow(expanded)),
    bbox_width = rep(0, nrow(expanded)),
    bbox_height = rep(0, nrow(expanded)),
    score = rep(1, nrow(expanded))
  )

  OpticsDetections(detection_data, source_file, ingest_format)
}

.optics_portal_standardize_upload <- function(df,
                                              role = c("model", "truth"),
                                              source_file = "uploaded.csv") {
  role <- match.arg(role)

  if (!is.data.frame(df) || nrow(df) == 0) {
    stop("Uploaded CSV must contain at least one row of data.", call. = FALSE)
  }

  names(df) <- trimws(names(df))
  category_col <- intersect(c("category_name", "class_label", "Species"), names(df))
  if (length(category_col) == 0) {
    stop("Uploaded CSV must contain `category_name` or `class_label`.", call. = FALSE)
  }
  category_col <- category_col[[1]]

  video_id <- if ("video_id" %in% names(df)) {
    as.character(df$video_id)
  } else {
    rep("uploaded_video", nrow(df))
  }

  image_id <- if ("image_id" %in% names(df)) {
    as.character(df$image_id)
  } else if ("frame_index" %in% names(df)) {
    paste0(video_id, "_frame_", df$frame_index)
  } else {
    stop("Uploaded CSV must contain `image_id` or `frame_index`.", call. = FALSE)
  }

  frame_index <- if ("frame_index" %in% names(df)) {
    suppressWarnings(as.numeric(df$frame_index))
  } else {
    match(image_id, unique(image_id))
  }

  if (any(is.na(frame_index))) {
    stop("`frame_index` values must be numeric when provided.", call. = FALSE)
  }

  detection_data <- dplyr::tibble(
    video_id = video_id,
    image_id = image_id,
    frame_index = frame_index,
    annotation_id = if ("annotation_id" %in% names(df)) {
      as.character(df$annotation_id)
    } else {
      paste0(role, "_", seq_len(nrow(df)))
    },
    category_name = as.character(df[[category_col]]),
    bbox_x = if ("bbox_x" %in% names(df)) suppressWarnings(as.numeric(df$bbox_x)) else rep(0, nrow(df)),
    bbox_y = if ("bbox_y" %in% names(df)) suppressWarnings(as.numeric(df$bbox_y)) else rep(0, nrow(df)),
    bbox_width = if ("bbox_width" %in% names(df)) suppressWarnings(as.numeric(df$bbox_width)) else rep(0, nrow(df)),
    bbox_height = if ("bbox_height" %in% names(df)) suppressWarnings(as.numeric(df$bbox_height)) else rep(0, nrow(df)),
    score = rep(1, nrow(df))
  )

  if (role == "model") {
    score_col <- intersect(c("score", "Confidence", "confidence"), names(df))
    if (length(score_col) == 0) {
      stop("Model upload must contain `score` or `Confidence`.", call. = FALSE)
    }
    score_col <- score_col[[1]]
    detection_data$score <- suppressWarnings(as.numeric(df[[score_col]]))
    if (any(is.na(detection_data$score))) {
      stop("Model confidence scores must be numeric.", call. = FALSE)
    }
  } else if ("true_count" %in% names(df) || "truth_count" %in% names(df)) {
    truth_count_col <- intersect(c("true_count", "truth_count"), names(df))[[1]]
    truth_counts <- dplyr::tibble(
      video_id = video_id,
      image_id = image_id,
      frame_index = frame_index,
      category_name = as.character(df[[category_col]]),
      truth_count = df[[truth_count_col]]
    )
    return(.optics_portal_truth_counts_to_detections(
      truth_counts = truth_counts,
      source_file = source_file,
      ingest_format = "uploaded_truth_counts"
    ))
  }

  OpticsDetections(detection_data, source_file, paste0("uploaded_", role))
}

.optics_portal_read_upload_file <- function(file_path,
                                            display_name,
                                            role = c("model", "truth")) {
  role <- match.arg(role)
  uploaded_df <- tryCatch(
    utils::read.csv(file_path, check.names = FALSE, stringsAsFactors = FALSE),
    error = function(e) {
      stop("Could not read uploaded ", role, " CSV: ", e$message, call. = FALSE)
    }
  )

  .optics_portal_standardize_upload(
    df = uploaded_df,
    role = role,
    source_file = display_name
  )
}

.optics_portal_load_example <- function(example_key) {
  extdata_dir <- system.file("extdata", package = "Optics")

  if (identical(example_key, "auv_frame_abundance")) {
    model_path <- file.path(extdata_dir, "NWFSC", "AUV_viame_test_detections.csv")
    truth_path <- file.path(extdata_dir, "NWFSC", "AUV_viame_test_groundtruth.csv")

    model_tmp <- .optics_portal_add_viame_headers(model_path)
    truth_tmp <- .optics_portal_add_viame_headers(truth_path)
    on.exit(unlink(c(model_tmp, truth_tmp)), add = TRUE)

    model_raw <- read_viame_csv(model_tmp, video_id = "AUV_viame_test")
    truth_raw <- read_viame_csv(truth_tmp, video_id = "AUV_viame_test")

    matched_images <- unique(truth_raw@data$image_id)
    model_data <- model_raw@data[model_raw@data$image_id %in% matched_images, , drop = FALSE]
    truth_data <- truth_raw@data[truth_raw@data$image_id %in% unique(model_data$image_id), , drop = FALSE]

    return(list(
      label = "AUV Frame Abundance",
      model_detections = OpticsDetections(model_data, model_path, "viame_csv_auv_model"),
      truth_detections = OpticsDetections(truth_data, truth_path, "viame_csv_auv_truth"),
      allowed_count_metrics = c("Frame Abundance", "MaxN"),
      default_count_metric = "Frame Abundance"
    ))
  }

  if (identical(example_key, "aerial_ice_seals")) {
    ext_dir <- file.path(extdata_dir, "AKFSC")
    model_files <- list.files(ext_dir, pattern = "_ir_detections\\.csv$", full.names = TRUE)
    truth_files <- list.files(ext_dir, pattern = "_validated\\.csv$", full.names = TRUE)
    extract_camera <- function(path) {
      camera <- stringr::str_match(basename(path), "_([LCR])_")[, 2]
      ifelse(is.na(camera), "", camera)
    }

    model_index <- dplyr::tibble(model_file = model_files, camera = extract_camera(model_files))
    truth_index <- dplyr::tibble(truth_file = truth_files, camera = extract_camera(truth_files))

    if (any(model_index$camera == "") || any(truth_index$camera == "")) {
      stop("Could not extract camera position (L/R/C) from one or more ice-seal files.", call. = FALSE)
    }
    if (anyDuplicated(model_index$camera) || anyDuplicated(truth_index$camera)) {
      stop("Expected one model and one validated ice-seal file per camera (L/R/C).", call. = FALSE)
    }

    camera_file_map <- dplyr::inner_join(model_index, truth_index, by = "camera")
    if (nrow(camera_file_map) == 0) {
      stop("No paired ice-seal model and validated files were found.", call. = FALSE)
    }

    model_detections <- ingest_ice_seals_csv(camera_file_map$model_file)
    truth_detections <- ingest_ice_seals_csv(camera_file_map$truth_file)

    extract_camera_from_image <- function(image_id) {
      image_name <- sub("^.*[\\\\/]", "", image_id)
      stringr::str_match(image_name, "_([LCR])_")[, 2]
    }

    model_data <- model_detections@data %>%
      dplyr::mutate(
        camera = extract_camera_from_image(.data$image_id),
        image_key = sub("^.*[\\\\/]", "", .data$image_id)
      )
    truth_data <- truth_detections@data %>%
      dplyr::mutate(
        camera = extract_camera_from_image(.data$image_id),
        image_key = sub("^.*[\\\\/]", "", .data$image_id)
      )

    truth_image_index <- truth_data %>% dplyr::distinct(.data$camera, .data$image_key)
    model_data <- model_data %>%
      dplyr::semi_join(truth_image_index, by = c("camera", "image_key"))
    model_image_index <- model_data %>% dplyr::distinct(.data$camera, .data$image_key)
    truth_data <- truth_data %>%
      dplyr::semi_join(model_image_index, by = c("camera", "image_key")) %>%
      dplyr::select(-.data$camera, -.data$image_key)
    model_data <- model_data %>% dplyr::select(-.data$camera, -.data$image_key)

    return(list(
      label = "Aerial Ice Seals",
      model_detections = OpticsDetections(model_data, paste(basename(model_files), collapse = ", "), "viame_csv_ice_seals_model"),
      truth_detections = OpticsDetections(truth_data, paste(basename(truth_files), collapse = ", "), "viame_csv_ice_seals_truth"),
      allowed_count_metrics = c("Frame Abundance", "MaxN"),
      default_count_metric = "Frame Abundance"
    ))
  }

  if (identical(example_key, "stationary_benthic_maxn")) {
    ext_dir <- file.path(extdata_dir, "SEFSC")
    truth_path <- file.path(ext_dir, "maxn3LABS_93to24.csv")
    species_path <- file.path(ext_dir, "Species List.csv")
    deployment_path <- file.path(ext_dir, "env3LABS_93to24.csv")
    track_files <- list.files(ext_dir, pattern = "_tracks.*\\.csv$", full.names = TRUE)

    species_lookup <- utils::read.csv(species_path, stringsAsFactors = FALSE)
    species_lookup$Spec_Viame_Dash <- trimws(as.character(species_lookup$Spec_Viame_Dash))
    species_lookup$Species <- toupper(trimws(as.character(species_lookup$Species)))

    deployment_lookup <- utils::read.csv(deployment_path, stringsAsFactors = FALSE)
    deployment_lookup$reference_compact <- .normalize_optics_portal_compact_id(deployment_lookup$REFERENCE)
    deployment_lookup$site_compact <- .normalize_optics_portal_compact_id(deployment_lookup$SITE_ID)

    model_parts <- lapply(track_files, function(track_file) {
      deployment_id <- sub("_tracks.*$", "", basename(track_file))
      detections <- read_viame_csv(track_file, video_id = deployment_id)
      model_df <- detections@data
      model_df$video_id <- .normalize_optics_portal_compact_id(model_df$video_id)
      mapped_species <- species_lookup$Species[match(trimws(model_df$category_name), species_lookup$Spec_Viame_Dash)]
      model_df$category_name <- ifelse(is.na(mapped_species) | mapped_species == "", model_df$category_name, mapped_species)
      model_df$category_name <- toupper(trimws(model_df$category_name))
      model_df
    })

    model_df <- dplyr::bind_rows(model_parts)
    truth_counts <- read_wide_maxn(truth_path, video_id_col = REFERENCE)
    truth_counts$video_id <- .normalize_optics_portal_compact_id(truth_counts$video_id)
    truth_counts$category_name <- toupper(trimws(truth_counts$category_name))
    model_deployments <- unique(model_df$video_id)
    truth_reference_match <- match(truth_counts$video_id, deployment_lookup$reference_compact)
    truth_site_match <- deployment_lookup$site_compact[truth_reference_match]
    use_site_match <- !is.na(truth_site_match) & truth_site_match %in% model_deployments
    truth_counts$video_id[use_site_match] <- truth_site_match[use_site_match]
    truth_counts <- truth_counts[truth_counts$video_id %in% model_deployments, , drop = FALSE]

    return(list(
      label = "Stationary Benthic MaxN",
      model_detections = OpticsDetections(model_df, paste(basename(track_files), collapse = ", "), "viame_csv_stationary_benthic_model"),
      truth_detections = .optics_portal_truth_counts_to_detections(
        truth_counts = truth_counts,
        source_file = truth_path,
        ingest_format = "wide_maxn_truth"
      ),
      allowed_count_metrics = "MaxN",
      default_count_metric = "MaxN"
    ))
  }

  stop("Unknown example selection: ", example_key, call. = FALSE)
}

.optics_portal_metric_details <- function(count_metric) {
  if (identical(count_metric, "Frame Abundance")) {
    return(list(
      metric_function = calculate_frame_abundance,
      join_by = c("video_id", "frame_index", "category_name"),
      count_col = "abundance"
    ))
  }

  list(
    metric_function = calculate_maxn,
    join_by = c("video_id", "category_name"),
    count_col = "maxn"
  )
}

.optics_portal_subset_detections <- function(detections, class_label = NULL) {
  if (is.null(class_label) || !nzchar(class_label)) {
    return(detections)
  }

  filtered_data <- detections@data[detections@data$category_name == class_label, , drop = FALSE]
  OpticsDetections(filtered_data, detections@source_file, detections@ingest_format)
}

.optics_portal_class_threshold_metrics <- function(model_detections,
                                                   truth_detections,
                                                   count_metric = "Frame Abundance",
                                                   class_label = NULL,
                                                   thresholds = .optics_portal_thresholds()) {
  metric_details <- .optics_portal_metric_details(count_metric)
  model_subset <- .optics_portal_subset_detections(model_detections, class_label)
  truth_subset <- .optics_portal_subset_detections(truth_detections, class_label)

  if (nrow(model_subset@data) == 0 && nrow(truth_subset@data) == 0) {
    return(dplyr::tibble(
      threshold = numeric(),
      Model = numeric(),
      Groundtruth = numeric(),
      Difference = numeric(),
      tp = numeric(),
      fp = numeric(),
      fn = numeric(),
      precision = numeric(),
      recall = numeric(),
      f1_score = numeric()
    ))
  }

  truth_counts_base <- metric_details$metric_function(truth_subset)

  purrr::map_dfr(thresholds, function(threshold) {
    model_filtered_df <- model_subset@data[model_subset@data$score >= threshold, , drop = FALSE]
    if (nrow(model_filtered_df) > 0) {
      model_filtered_df$score <- threshold
    }

    filtered_model <- OpticsDetections(
      model_filtered_df,
      model_subset@source_file,
      paste0(model_subset@ingest_format, "_filtered")
    )

    model_counts <- metric_details$metric_function(filtered_model)
    truth_counts <- truth_counts_base

    model_counts$score <- threshold
    truth_counts$score <- threshold
    model_counts$count_value <- model_counts[[metric_details$count_col]]
    truth_counts$count_value <- truth_counts[[metric_details$count_col]]

    aligned_counts <- align_counts(
      model_counts = model_counts,
      truth_counts = truth_counts,
      by = unique(c(metric_details$join_by, "score")),
      model_col = count_value,
      truth_col = count_value
    )

    tp <- sum(aligned_counts$model_count > 0 & aligned_counts$truth_count > 0, na.rm = TRUE)
    fp <- sum(aligned_counts$model_count > 0 & aligned_counts$truth_count == 0, na.rm = TRUE)
    fn <- sum(aligned_counts$model_count == 0 & aligned_counts$truth_count > 0, na.rm = TRUE)
    metrics <- .scalpred_prf_from_counts(tp = tp, fp = fp, fn = fn)

    dplyr::bind_cols(
      dplyr::tibble(
        threshold = threshold,
        Model = sum(aligned_counts$model_count, na.rm = TRUE),
        Groundtruth = sum(aligned_counts$truth_count, na.rm = TRUE),
        Difference = sum(aligned_counts$truth_count, na.rm = TRUE) - sum(aligned_counts$model_count, na.rm = TRUE)
      ),
      metrics
    )
  })
}

.optics_portal_analyze <- function(model_detections,
                                   truth_detections,
                                   count_metric = "Frame Abundance",
                                   threshold = 0.5) {
  metric_details <- .optics_portal_metric_details(count_metric)
  thresholds <- .optics_portal_thresholds(threshold)

  performance_summary <- summarize_performance_by_threshold(
    model_detections = model_detections,
    truth_detections = truth_detections,
    by = metric_details$join_by,
    metric_function = metric_details$metric_function,
    thresholds = thresholds
  )

  scalpred_summary <- calculate_scalpred_metrics(
    model_detections = model_detections,
    truth_detections = truth_detections,
    by = metric_details$join_by,
    thresholds = thresholds,
    metric_function = metric_details$metric_function
  )

  model_filtered_df <- model_detections@data[model_detections@data$score >= threshold, , drop = FALSE]
  if (nrow(model_filtered_df) > 0) {
    model_filtered_df$score <- threshold
  }

  filtered_model <- OpticsDetections(
    model_filtered_df,
    model_detections@source_file,
    paste0(model_detections@ingest_format, "_filtered")
  )

  model_counts <- metric_details$metric_function(filtered_model)
  truth_counts <- metric_details$metric_function(truth_detections)

  model_counts$score <- threshold
  truth_counts$score <- threshold
  model_counts$count_value <- model_counts[[metric_details$count_col]]
  truth_counts$count_value <- truth_counts[[metric_details$count_col]]

  aligned_counts <- align_counts(
    model_counts = model_counts,
    truth_counts = truth_counts,
    by = unique(c(metric_details$join_by, "score")),
    model_col = count_value,
    truth_col = count_value
  )

  threshold_index <- which.min(abs(performance_summary$threshold - threshold))
  selected_metrics <- performance_summary[threshold_index, , drop = FALSE]
  confusion_groups <- setdiff(metric_details$join_by, "category_name")

  list(
    performance_summary = performance_summary,
    scalpred_summary = scalpred_summary,
    aligned_counts = aligned_counts,
    selected_metrics = selected_metrics,
    multiclass_confusion = calculate_confusion_matrix(
      aligned_counts,
      group_vars = confusion_groups,
      species_col = category_name
    )
  )
}

optics_portal_data_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::uiOutput(ns("example_picker")),
    shiny::uiOutput(ns("data_status"))
  )
}

optics_portal_data_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    example_catalog <- .discover_optics_portal_examples()

    output$example_picker <- shiny::renderUI({
      example_choices <- stats::setNames(example_catalog$key, example_catalog$label)
      if (length(example_choices) == 0) {
        return(shiny::helpText("No bundled example datasets were found in inst/extdata."))
      }

      shiny::selectInput(
        session$ns("example_key"),
        "Bundled pipeline",
        choices = example_choices,
        selected = example_catalog$key[[1]]
      )
    })

    loaded_data <- shiny::reactive({
      shiny::req(input$example_key)
      tryCatch(
        .optics_portal_load_example(input$example_key),
        error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error")
          NULL
        }
      )
    })

    output$data_status <- shiny::renderUI({
      current_data <- loaded_data()
      if (is.null(current_data)) {
        return(NULL)
      }

      shiny::tags$div(
        class = "small text-muted",
        paste0(
          current_data$label,
          ": ",
          nrow(current_data$model_detections@data),
          " model rows / ",
          nrow(current_data$truth_detections@data),
          " truth rows loaded."
        )
      )
    })

    list(
      dataset = loaded_data,
      examples = example_catalog
    )
  })
}

optics_portal_ui <- function() {
  bslib::page_sidebar(
    title = "Optics Portal",
    theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
    sidebar = bslib::sidebar(
      optics_portal_data_ui("data"),
      shiny::uiOutput("count_metric_ui"),
      shiny::uiOutput("class_selector_ui"),
      shiny::selectInput(
        "view_selection",
        "Figure view",
        choices = c(
          "Performance summary" = "performance",
          "Precision-Recall curve" = "pr_curve",
          "F1 score" = "f1_curve",
          "Class-level confusion matrix" = "multiclass_confusion",
          "Aligned model vs. ground truth" = "aligned_data"
        ),
        selected = "performance"
      ),
      shiny::sliderInput(
        "confidence_threshold",
        "Confidence threshold",
        min = 0,
        max = 1,
        value = 0.5,
        step = 0.05
      ),
      shiny::checkboxGroupInput(
        "performance_metrics",
        "Performance metrics",
        choices = c("Precision" = "precision", "Recall" = "recall", "F1 Score" = "f1_score"),
        selected = c("precision", "recall", "f1_score")
      )
    ),
    shiny::tabsetPanel(
      id = "main_view",
      type = "hidden",
      shiny::tabPanelBody(
        "performance",
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Performance summary"),
          plotly::plotlyOutput("performance_plot", height = "420px"),
          DT::DTOutput("selected_metric_table")
        )
      ),
      shiny::tabPanelBody(
        "pr_curve",
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Precision-Recall curve"),
          plotly::plotlyOutput("pr_curve_plot", height = "520px")
        )
      ),
      shiny::tabPanelBody(
        "f1_curve",
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("F1 score"),
          plotly::plotlyOutput("f1_curve_plot", height = "520px")
        )
      ),
      shiny::tabPanelBody(
        "multiclass_confusion",
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Class-level confusion matrix"),
          plotly::plotlyOutput("multiclass_confusion_plot", height = "600px")
        )
      ),
      shiny::tabPanelBody(
        "aligned_data",
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Aligned model vs. ground truth"),
          DT::DTOutput("aligned_data_table")
        )
      )
    )
  )
}

optics_portal_server <- function(input, output, session) {
  data_source <- optics_portal_data_server("data")

  shiny::observeEvent(input$view_selection, {
    shiny::updateTabsetPanel(session, "main_view", selected = input$view_selection)
  }, ignoreNULL = FALSE)

  output$count_metric_ui <- shiny::renderUI({
    current_data <- data_source$dataset()
    available_metrics <- if (is.null(current_data)) {
      c("Frame Abundance", "MaxN")
    } else {
      current_data$allowed_count_metrics
    }
    selected_metric <- if (is.null(current_data)) {
      available_metrics[[1]]
    } else {
      current_data$default_count_metric
    }

    shiny::selectInput(
      "count_metric",
      "Count metric",
      choices = available_metrics,
      selected = selected_metric
    )
  })

  output$class_selector_ui <- shiny::renderUI({
    current_data <- data_source$dataset()
    shiny::req(current_data)

    class_choices <- sort(unique(c(
      as.character(current_data$model_detections@data$category_name),
      as.character(current_data$truth_detections@data$category_name)
    )))
    class_choices <- class_choices[nzchar(class_choices)]

    shiny::selectInput(
      "class_label",
      "Classification",
      choices = class_choices,
      selected = class_choices[[1]]
    )
  })

  analysis_results <- shiny::reactive({
    current_data <- data_source$dataset()
    shiny::req(current_data, input$count_metric)

    tryCatch(
      .optics_portal_analyze(
        model_detections = current_data$model_detections,
        truth_detections = current_data$truth_detections,
        count_metric = input$count_metric,
        threshold = input$confidence_threshold
      ),
      error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
        NULL
      }
    )
  })

  class_threshold_results <- shiny::reactive({
    current_data <- data_source$dataset()
    shiny::req(current_data, input$count_metric, input$class_label)

    tryCatch(
      .optics_portal_class_threshold_metrics(
        model_detections = current_data$model_detections,
        truth_detections = current_data$truth_detections,
        count_metric = input$count_metric,
        class_label = input$class_label,
        thresholds = .optics_portal_thresholds(input$confidence_threshold)
      ),
      error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
        NULL
      }
    )
  })

  selected_class_metrics <- shiny::reactive({
    results <- class_threshold_results()
    shiny::req(results, nrow(results) > 0)
    results[which.min(abs(results$threshold - input$confidence_threshold)), , drop = FALSE]
  })

  output$performance_plot <- plotly::renderPlotly({
    results <- selected_class_metrics()
    shiny::req(results, length(input$performance_metrics) > 0)

    plot_data <- results %>%
      dplyr::select(dplyr::all_of(input$performance_metrics)) %>%
      dplyr::mutate(row_id = 1L) %>%
      tidyr::pivot_longer(-.data$row_id, names_to = "metric", values_to = "value")

    plotly::ggplotly(
      ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$metric, y = .data$value, fill = .data$metric)) +
        ggplot2::geom_col(width = 0.6, show.legend = FALSE) +
        ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
        ggplot2::labs(
          title = paste0(input$class_label, " performance"),
          subtitle = paste0("Confidence threshold: ", format(input$confidence_threshold, trim = TRUE)),
          x = NULL,
          y = "Metric value"
        ) +
        theme_optics(),
      tooltip = c("x", "y")
    )
  })

  output$selected_metric_table <- DT::renderDT({
    results <- selected_class_metrics()
    shiny::req(results)

    metric_columns <- c("threshold", "Model", "Groundtruth", "Difference", input$performance_metrics)
    metric_columns <- metric_columns[metric_columns %in% names(results)]

    DT::datatable(
      results[, metric_columns, drop = FALSE],
      options = list(pageLength = 1, dom = "tip", scrollX = TRUE),
      rownames = FALSE
    )
  })

  output$pr_curve_plot <- plotly::renderPlotly({
    results <- class_threshold_results()
    shiny::req(results)

    plotly::ggplotly(
      plot_pr_curve(results, title = paste0(input$class_label, " precision-recall curve")),
      tooltip = c("x", "y")
    )
  })

  output$f1_curve_plot <- plotly::renderPlotly({
    results <- selected_class_metrics()
    shiny::req(results)

    plotly::ggplotly(
      ggplot2::ggplot(results, ggplot2::aes(x = "F1 Score", y = .data$f1_score)) +
        ggplot2::geom_col(width = 0.45, fill = "#4E79A7") +
        ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
        ggplot2::labs(
          title = paste0(input$class_label, " F1 score"),
          subtitle = paste0("Confidence threshold: ", format(input$confidence_threshold, trim = TRUE)),
          x = NULL,
          y = "F1 score"
        ) +
        theme_optics(),
      tooltip = c("x", "y")
    )
  })

  output$multiclass_confusion_plot <- plotly::renderPlotly({
    results <- analysis_results()
    shiny::req(results)
    plotly::ggplotly(plot_multiclass_confusion_matrix(results$multiclass_confusion))
  })

  output$aligned_data_table <- DT::renderDT({
    results <- class_threshold_results()
    shiny::req(results)

    DT::datatable(
      results,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
}
