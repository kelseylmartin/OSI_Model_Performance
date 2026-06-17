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
#' # Example using a sample of GFISHER data to calculate MaxN.
#' gfisher_csv_data <- c(
#'   "1,video1,10,100,100,200,200,1,0.95,"Gadus morhua",1",
#'   "1,video1,10,150,150,250,250,1,0.90,"Gadus morhua",1",
#'   "2,video1,10,300,300,400,400,1,0.85,"Melanogrammus aeglefinus",1",
#'   "1,video1,11,100,100,200,200,1,0.92,"Gadus morhua",1"
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
#' # Example using a sample of GFISHER data to calculate per-frame abundance.
#' # This simulates reading a VIAME CSV output from a GFISHER survey.
#' gfisher_csv_data <- c(
#'   "1,video1,10,100,100,200,200,1,0.95,"Gadus morhua",1",
#'   "1,video1,10,150,150,250,250,1,0.90,"Gadus morhua",1",
#'   "2,video1,10,300,300,400,400,1,0.85,"Melanogrammus aeglefinus",1",
#'   "1,video1,11,100,100,200,200,1,0.92,"Gadus morhua",1"
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
