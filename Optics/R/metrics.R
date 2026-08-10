#' @include ingest.R
NULL

.normalize_detection_schema <- function(detections_df, group_cols = NULL) {
  required_cols <- c("video_id", "frame_index", "category_name")
  legacy_cols <- c("VidIdent", "UniqFrame", "SP")

  if (!all(required_cols %in% names(detections_df)) &&
      all(legacy_cols %in% names(detections_df))) {
    detections_df <- dplyr::rename(
      detections_df,
      video_id = "VidIdent",
      frame_index = "UniqFrame",
      category_name = "SP"
    )

    if (!is.null(group_cols)) {
      group_cols <- dplyr::recode(
        group_cols,
        VidIdent = "video_id",
        UniqFrame = "frame_index",
        SP = "category_name"
      )
    }
  }

  list(detections_df = detections_df, group_cols = group_cols)
}

.ensure_score_column <- function(detections_df) {
  if (!"score" %in% names(detections_df)) {
    if ("Confidence" %in% names(detections_df)) {
      detections_df$score <- suppressWarnings(as.numeric(detections_df$Confidence))
    } else {
      stop("Input data frame must contain a 'score' column.")
    }
  }
  stopifnot("score" %in% colnames(detections_df))
  detections_df
}

#' Calculate MaxN (Maximum Number of Individuals)
#'
#' Calculates the maximum number of individuals of each species category
#' observed in any single frame within each video. This is a standard metric
#' for baited remote underwater video (BRUV) surveys.
#'
#' @param object An object containing detection data.
#' @param ... Additional arguments passed to methods.
#'
#' @return A `tibble` with columns for each grouping variable and a `maxn`
#'   column containing the calculated MaxN value.
#'
#' @export
#' @rdname calculate_maxn
setGeneric("calculate_maxn", function(object, ...) {
  standardGeneric("calculate_maxn")
})

#' @param group_cols A character vector of additional column names to group by.
#'
#' @rdname calculate_maxn
#' @export
#' @importFrom dplyr group_by summarise n
#' @importFrom rlang .data syms
#' @importFrom magrittr %>%
#' @examples
#' # Example using a sample of GFISHER data to calculate MaxN.
#' gfisher_csv_data <- c(
#'   "1,video1,10,100,100,200,200,1,0.95,\"Gadus morhua\",1",
#'   "1,video1,10,150,150,250,250,1,0.90,\"Gadus morhua\",1",
#'   "2,video1,10,300,300,400,400,1,0.85,\"Melanogrammus aeglefinus\",1",
#'   "1,video1,11,100,100,200,200,1,0.92,\"Gadus morhua\",1"
#' )
#' temp_csv_path <- tempfile(fileext = ".csv")
#' writeLines(c("# header 1", "# header 2", gfisher_csv_data), temp_csv_path)
#'
#' # Ingest the data
#' detections_obj <- read_viame_csv(temp_csv_path)
#'
#' # Calculate MaxN
#' maxn_df <- calculate_maxn(detections_obj)
#' print(maxn_df)
#'
#' # Clean up the temporary file
#' unlink(temp_csv_path)
#'
#' \dontrun{
#' # For GFISHER survey, calculate MaxN using a file from the GFISHER folder
#' gfisher_csv <- "Data/GFISHER/762301061_cam3_tracks.csv"
#' detections <- read_viame_csv(gfisher_csv)
#'
#' # Calculate MaxN
#' maxn_df <- calculate_maxn(detections)
#' print(maxn_df)
#' }
setMethod("calculate_maxn", "OpticsDetections",
          function(object, group_cols = NULL) {
            
            detections_df <- object@data
            detections_df <- .ensure_score_column(detections_df)
            # --- 1. Input Validation ---
            required_cols <- c("video_id", "frame_index", "category_name", "score")
            if (!all(required_cols %in% names(detections_df))) {
              stop("Input data frame must contain columns: ", paste(required_cols, collapse = ", "))
            }
            
            all_groups <- unique(c("video_id", "category_name", "score", group_cols))
            
            if (nrow(detections_df) == 0) {
              return(dplyr::tibble(!!!stats::setNames(lapply(c(all_groups, "maxn"), function(x) logical(0)), c(all_groups, "maxn"))))
            }
            
            # --- 2. Calculate MaxN ---
            detections_df %>%
              dplyr::group_by(!!!rlang::syms(unique(c(all_groups, "frame_index")))) %>%
              dplyr::summarise(n_in_frame = dplyr::n(), .groups = "drop") %>%
              dplyr::group_by(!!!rlang::syms(all_groups)) %>%
              dplyr::summarise(maxn = max(c(0, .data$n_in_frame)), .groups = "drop")
          })

#' @rdname calculate_maxn
#' @export
setMethod("calculate_maxn", "data.frame",
          function(object, group_cols = NULL) {
            
            normalized <- .normalize_detection_schema(object, group_cols = group_cols)
            detections_df <- normalized$detections_df
            group_cols <- normalized$group_cols
            detections_df <- .ensure_score_column(detections_df)
            # --- 1. Input Validation ---
            required_cols <- c("video_id", "frame_index", "category_name", "score")
            if (!all(required_cols %in% names(detections_df))) {
              stop("Input data frame must contain columns: ", paste(required_cols, collapse = ", "))
            }
            
            all_groups <- unique(c("video_id", "category_name", "score", group_cols))
            
            if (nrow(detections_df) == 0) {
              return(dplyr::tibble(!!!stats::setNames(lapply(c(all_groups, "maxn"), function(x) logical(0)), c(all_groups, "maxn"))))
            }
            
            # --- 2. Calculate MaxN ---
            detections_df %>%
              dplyr::group_by(!!!rlang::syms(unique(c(all_groups, "frame_index")))) %>%
              dplyr::summarise(n_in_frame = dplyr::n(), .groups = "drop") %>%
              dplyr::group_by(!!!rlang::syms(all_groups)) %>%
              dplyr::summarise(maxn = max(c(0, .data$n_in_frame)), .groups = "drop")
          })


#' Calculate Frame-by-Frame Abundance
#'
#' Calculates the number of detections for each species category in every frame
#' where they appear.
#'
#' @param object An object containing detection data.
#' @param ... Additional arguments passed to methods.
#'
#' @return A `tibble` with per-frame counts.
#' @export
#' @rdname calculate_frame_abundance
setGeneric("calculate_frame_abundance", function(object, ...) {
  standardGeneric("calculate_frame_abundance")
})

#' @param group_cols A character vector of additional column names to group by.
#'
#' @rdname calculate_frame_abundance
#' @export
#' @importFrom dplyr group_by summarise n
#' @importFrom rlang syms
#' @importFrom magrittr %>%
#' @examples
#' # Example using a sample of GFISHER data to calculate per-frame abundance.
#' # This simulates reading a VIAME CSV output from a GFISHER survey.
#' gfisher_csv_data <- c(
#'   "1,video1,10,100,100,200,200,1,0.95,\"Gadus morhua\",1",
#'   "1,video1,10,150,150,250,250,1,0.90,\"Gadus morhua\",1",
#'   "2,video1,10,300,300,400,400,1,0.85,\"Melanogrammus aeglefinus\",1",
#'   "1,video1,11,100,100,200,200,1,0.92,\"Gadus morhua\",1"
#' )
#' temp_csv_path <- tempfile(fileext = ".csv")
#' writeLines(c("# header 1", "# header 2", gfisher_csv_data), temp_csv_path)
#'
#' # Ingest the data
#' detections_obj <- read_viame_csv(temp_csv_path, model_name = "GFISHER-model")
#'
#' # Calculate abundance per frame
#' calculate_frame_abundance(detections_obj)
#'
#' # Clean up the temporary file
#' unlink(temp_csv_path)
setMethod("calculate_frame_abundance", "OpticsDetections",
          function(object, group_cols = NULL) {
            
            detections_df <- object@data
            detections_df <- .ensure_score_column(detections_df)
            # --- 1. Input Validation ---
            required_cols <- c("video_id", "frame_index", "category_name", "score")
            if (!all(required_cols %in% names(detections_df))) {
              stop("Input data frame must contain columns: ", paste(required_cols, collapse = ", "))
            }
            
            all_groups <- unique(c("video_id", "frame_index", "category_name", "score", group_cols))
            
            if (nrow(detections_df) == 0) {
              return(dplyr::tibble(!!!stats::setNames(lapply(c(all_groups, "abundance"), function(x) logical(0)), c(all_groups, "abundance"))))
            }
            
            # --- 2. Calculate Abundance ---
            detections_df %>%
              dplyr::group_by(!!!rlang::syms(all_groups)) %>%
              dplyr::summarise(abundance = dplyr::n(), .groups = "drop")
          })

#' @rdname calculate_frame_abundance
#' @export
setMethod("calculate_frame_abundance", "data.frame",
          function(object, group_cols = NULL) {
            
            normalized <- .normalize_detection_schema(object, group_cols = group_cols)
            detections_df <- normalized$detections_df
            group_cols <- normalized$group_cols
            detections_df <- .ensure_score_column(detections_df)
            # --- 1. Input Validation ---
            required_cols <- c("video_id", "frame_index", "category_name", "score")
            if (!all(required_cols %in% names(detections_df))) {
              stop("Input data frame must contain columns: ", paste(required_cols, collapse = ", "))
            }
            
            all_groups <- unique(c("video_id", "frame_index", "category_name", "score", group_cols))
            
            if (nrow(detections_df) == 0) {
              return(dplyr::tibble(!!!stats::setNames(lapply(c(all_groups, "abundance"), function(x) logical(0)), c(all_groups, "abundance"))))
            }
            
            # --- 2. Calculate Abundance ---
            detections_df %>%
              dplyr::group_by(!!!rlang::syms(all_groups)) %>%
              dplyr::summarise(abundance = dplyr::n(), .groups = "drop")
          })


#' Calculate Density from Counts and Area
#'
#' Calculates density by dividing a count by an area. This generic function
#' dispatches to methods based on the class of the input object.
#'
#' @param object A data frame or other object containing count data.
#' @param ... Additional arguments passed to methods.
#'
#' @return The input object with an added `density` column.
#' @export
#' @rdname calculate_density
setGeneric("calculate_density", function(object, ...) {
  standardGeneric("calculate_density")
})

#' @param count_col The unquoted name of the column containing the count data.
#' @param area A single numeric value for the survey area.
#' @param area_col An optional unquoted column name for area values.
#' @rdname calculate_density
#' @export
#' @importFrom dplyr mutate
#' @importFrom rlang enquo quo_is_null
#' @examples
#' # Example for Tom & Michael's coral survey data.
#' # Create a data frame with coral counts per site.
#' coral_counts_df <- dplyr::tibble(
#'   site = c("site1", "site1", "site2"),
#'   taxon = c("Acropora", "Pocillopora", "Acropora"),
#'   count = c(50, 25, 30),
#'   survey_area_m2 = c(10, 10, 8)
#' )
#'
#' # Calculate density using a fixed area for all sites.
#' density_fixed_area <- calculate_density(
#'   coral_counts_df,
#'   count_col = count,
#'   area = 100
#' )
#' print(density_fixed_area)
#'
#' # Calculate density using a per-site area from a column.
#' density_per_site_area <- calculate_density(
#'   coral_counts_df,
#'   count_col = count,
#'   area_col = survey_area_m2
#' )
#' print(density_per_site_area)
setMethod("calculate_density", "data.frame",
          function(object, count_col, area = NULL, area_col = NULL) {
            count_col_quo <- rlang::enquo(count_col)
            area_col_quo <- rlang::enquo(area_col)
            
            if (!is.null(area) && !rlang::quo_is_null(area_col_quo)) {
              stop("Please provide 'area' or 'area_col', but not both.")
            }
            
            if (is.null(area) && rlang::quo_is_null(area_col_quo)) {
              stop("Please provide either 'area' or 'area_col'.")
            }
            
            if (!is.null(area)) {
              return(dplyr::mutate(object, density = !!count_col_quo / area))
            } else {
              return(dplyr::mutate(object, density = !!count_col_quo / !!area_col_quo))
            }
          })

#' Calculate Legacy GFisher Summary Metrics
#'
#' Produces the legacy-style summary table used by the GFisher workflow while
#' relying on package metric calculations for binary-count performance terms.
#'
#' @param df A data frame containing at least `Manual`, `VIAME_MaxN`, `year`,
#'   `Version`, `Confidence`, and `Species`.
#' @param species Character filter mode: `"none"`, `"all"`, or a single species
#'   name.
#'
#' @return A `tibble` containing legacy metric columns.
#' @export
calculate_legacy_metrics <- function(df, species = "none") {
  required_cols <- c("Manual", "VIAME_MaxN", "year", "Version", "Confidence", "Species")
  if (!all(required_cols %in% names(df))) {
    stop("Input data frame must contain columns: ", paste(required_cols, collapse = ", "))
  }

  if (species == "none") {
    group_vars <- c("year", "Version", "Confidence")
    df_in <- df
  } else if (species == "all") {
    group_vars <- c("year", "Version", "Confidence", "Species")
    df_in <- df
  } else if (any(species %in% df$Species) == TRUE) {
    group_vars <- c("year", "Version", "Confidence", "Species")
    df_in <- dplyr::filter(df, .data$Species == species)
  } else {
    print("No species detected with that name. Check spelling and try again.")
    return(NULL)
  }

  df_in %>%
    dplyr::mutate(
      Agree = ifelse(.data$Manual == .data$VIAME_MaxN, 1, 0),
      Difference = .data$Manual - .data$VIAME_MaxN,
      Relaxed = ifelse(.data$Difference %in% c(-1, 0, 1), 1, 0)
    ) %>%
    dplyr::group_by(!!!rlang::syms(group_vars)) %>%
    dplyr::group_modify(~ {
      binary <- calculate_binary_metrics(
        .x %>% dplyr::transmute(model_count = .data$VIAME_MaxN, truth_count = .data$Manual),
        total_comparisons = nrow(.x)
      )

      dplyr::tibble(
        Agree = mean(.x$Agree, na.rm = TRUE),
        Difference = mean(.x$Difference, na.rm = TRUE),
        Relaxed = mean(.x$Relaxed, na.rm = TRUE),
        TP = binary$tp,
        FP = binary$fp,
        FN = binary$fn,
        TN = binary$tn,
        Precision = binary$precision,
        Recall_TPR = binary$recall,
        FPR = binary$fpr,
        FNR = binary$fnr,
        Accuracy = binary$accuracy,
        False_P_Ratio = binary$false_positive_ratio,
        False_N_Ratio = binary$false_negative_ratio,
        Total_Actual_Positives = binary$tp + binary$fn,
        Total_Actual_Negatives = binary$fp + binary$tn
      )
    }) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(across(where(is.numeric), ~ ifelse(is.nan(.x), NA, .x))) %>%
    dplyr::mutate(across(where(is.numeric), ~ ifelse(is.infinite(.x), NA, .x)))
}
