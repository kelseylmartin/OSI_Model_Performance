#' @include ingest.R
NULL

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
#' # Create a sample OpticsDetections object
#' sample_df <- dplyr::tibble(
#'   video_id = c(rep("video1", 4), rep("video2", 3)),
#'   frame_index = c(1, 1, 2, 2, 1, 1, 1),
#'   category_name = c("FishA", "FishA", "FishA", "FishB", "FishA", "FishA", "FishA")
#' )
#' detections_obj <- OpticsDetections(
#'   data = sample_df,
#'   source_file = "dummy_file.csv",
#'   ingest_format = "dummy"
#' )
#'
#' # Calculate MaxN for each video and species
#' calculate_maxn(detections_obj)
#' 
#' \dontrun{
#' # For Erin's ice seal survey, calculate MaxN by camera view
#' erin_csv_c <- system.file("extdata", 
#' "ice_seals_2025_fl223_C_rgb_irDetectionsTransposed_processed.csv", package = "Optics")
#' detections_c <- read_viame_csv(erin_csv_c, video_id = "center")
#' 
#' erin_csv_l <- system.file("extdata", 
#' "ice_seals_2025_fl223_L_rgb_irDetectionsTransposed_processed.csv", package = "Optics")
#' detections_l <- read_viame_csv(erin_csv_l, video_id = "left")
#' 
#' erin_csv_r <- system.file("extdata", 
#' "ice_seals_2025_fl223_R_rgb_irDetectionsTransposed_processed.csv", package = "Optics")
#' detections_r <- read_viame_csv(erin_csv_r, video_id = "right")
#' 
#' all_detections <- rbind(detections_c@data, detections_l@data, detections_r@data)
#' 
#' maxn <- calculate_maxn(all_detections, group_cols = "video_id")
#' }
#'
setMethod("calculate_maxn", "OpticsDetections",
          function(object, group_cols = NULL) {
            
            detections_df <- object@data
            # --- 1. Input Validation ---
            required_cols <- c("video_id", "frame_index", "category_name")
            if (!all(required_cols %in% names(detections_df))) {
              stop("Input data frame must contain columns: ", paste(required_cols, collapse = ", "))
            }
            
            all_groups <- unique(c("video_id", "category_name", group_cols))
            
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
            
            detections_df <- object
            # --- 1. Input Validation ---
            required_cols <- c("video_id", "frame_index", "category_name")
            if (!all(required_cols %in% names(detections_df))) {
              stop("Input data frame must contain columns: ", paste(required_cols, collapse = ", "))
            }
            
            all_groups <- unique(c("video_id", "category_name", group_cols))
            
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
#' # Create a sample OpticsDetections object
#' sample_df <- dplyr::tibble(
#'   video_id = c(rep("video1", 3)),
#'   frame_index = c(1, 1, 2),
#'   category_name = c("FishA", "FishA", "FishB")
#' )
#' detections_obj <- OpticsDetections(
#'   data = sample_df,
#'   source_file = "dummy_file.csv",
#'   ingest_format = "dummy"
#' )
#'
#' # Calculate frame-by-frame abundance
#' calculate_frame_abundance(detections_obj)
#'
#' \dontrun{
#' # For Erin's ice seal survey
#' erin_csv <- system.file("extdata", 
#' "ice_seals_2025_fl223_C_rgb_irDetectionsTransposed_processed.csv", package = "Optics")
#' detections <- read_viame_csv(erin_csv)
#' fba <- calculate_frame_abundance(detections)
#' }
setMethod("calculate_frame_abundance", "OpticsDetections",
          function(object, group_cols = NULL) {
            
            detections_df <- object@data
            # --- 1. Input Validation ---
            required_cols <- c("video_id", "frame_index", "category_name")
            if (!all(required_cols %in% names(detections_df))) {
              stop("Input data frame must contain columns: ", paste(required_cols, collapse = ", "))
            }
            
            all_groups <- unique(c("video_id", "frame_index", "category_name", group_cols))
            
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
            
            detections_df <- object
            # --- 1. Input Validation ---
            required_cols <- c("video_id", "frame_index", "category_name")
            if (!all(required_cols %in% names(detections_df))) {
              stop("Input data frame must contain columns: ", paste(required_cols, collapse = ", "))
            }
            
            all_groups <- unique(c("video_id", "frame_index", "category_name", group_cols))
            
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
#' count_data <- dplyr::tibble(
#'   site = c("A", "B"),
#'   maxn = c(10, 25),
#'   survey_area_m2 = c(5, 10)
#' )
#'
#' # Using a fixed area
#' calculate_density(count_data, count_col = maxn, area = 100)
#'
#' # Using a per-site area from a column
#' calculate_density(count_data, count_col = maxn, area_col = survey_area_m2)
#' 
#' \dontrun{
#' # For Tom & Michael's coral survey
#' # Assuming you have a data frame with counts per taxon
#' coral_counts <- dplyr::tibble(
#'  taxon = c("Acropora", "Pocillopora"),
#'  count = c(50, 25)
#'  )
#' # Calculate density with a fixed sampling area
#' density <- calculate_density(coral_counts, count_col = count, area = 500)
#' }
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

