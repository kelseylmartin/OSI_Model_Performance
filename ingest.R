#' Read and Flatten a KWCOCO JSON file
#'
#' Ingests a KWCOCO-formatted JSON file and converts it into a standardized
#' tibble where each row represents a single object detection.
#'
#' @param file_path The full path to the KWCOCO JSON file.
#' @return A `tibble` containing the flattened annotation data, conforming to
#'   the package's internal standard schema. Returns an empty tibble if the
#'   file is invalid, cannot be parsed, or contains no annotations.
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr tibble as_tibble inner_join select rename all_of
#' @importFrom tidyr unnest_wider
#' @examples
#' \dontrun{
#'   # Create a dummy KWCOCO file for demonstration
#'   dummy_kwcoco <- list(
#'     videos = list(list(id = 1, name = "video_1")),
#'     images = list(
#'       list(id = 101, video_id = 1, file_name = "frame_001.jpg", frame_index = 1),
#'       list(id = 102, video_id = 1, file_name = "frame_002.jpg", frame_index = 2)
#'     ),
#'     annotations = list(
#'       list(id = 1, image_id = 101, category_id = 1, track_id = 1,
#'            bbox = c(10, 20, 30, 40), score = 0.95, area = 1200)
#'     ),
#'     categories = list(list(id = 1, name = "Gadus morhua"))
#'   )
#'   json_path <- tempfile(fileext = ".json")
#'   jsonlite::write_json(dummy_kwcoco, json_path, auto_unbox = TRUE)
#'
#'   # Ingest the file
#'   detections_df <- read_kwcoco(json_path)
#'   print(detections_df)
#'
#'   # Clean up
#'   unlink(json_path)
#' }
read_kwcoco <- function(file_path) {

  # --- 1. Input Validation and Reading ---
  if (!file.exists(file_path)) {
    warning("File does not exist: ", file_path)
    return(dplyr::tibble())
  }

  kwcoco_data <- tryCatch({
    jsonlite::fromJSON(file_path, simplifyDataFrame = FALSE)
  }, error = function(e) {
    warning("Failed to parse JSON file: ", file_path, " - Error: ", e$message)
    return(NULL)
  })

  if (is.null(kwcoco_data) || length(kwcoco_data$annotations) == 0) {
    warning("KWCOCO file is empty or contains no annotations: ", basename(file_path))
    return(dplyr::tibble())
  }

  # --- 2. Create Lookup Tables ---
  # Use `as_tibble` for robust handling of potentially empty lists
  images_df <- dplyr::as_tibble(kwcoco_data$images)
  categories_df <- dplyr::as_tibble(kwcoco_data$categories)
  videos_df <- dplyr::as_tibble(kwcoco_data$videos)

  # --- 3. Process Annotations ---
  # `as_tibble` handles list of lists better
  annotations_df <- dplyr::as_tibble(kwcoco_data$annotations) %>%
    tidyr::unnest_wider(bbox, names_sep = "_") # Flattens the bbox list column

  # --- 4. Join and Standardize ---
  # Left join from annotations to ensure all detections are kept
  # Use `suppressMessages` to hide join messages in a package context
  flat_df <- suppressMessages({
    annotations_df %>%
      dplyr::inner_join(images_df, by = c("image_id" = "id")) %>%
      dplyr::inner_join(categories_df, by = c("category_id" = "id")) %>%
      dplyr::inner_join(videos_df, by = c("video_id" = "id"))
  })

  # Rename to the internal standard schema
  # Use `any_of` to avoid errors if optional columns (like track_id) are missing
  standardized_df <- flat_df %>%
    dplyr::select(dplyr::any_of(c(
      video_id = "name.y", image_id = "file_name", frame_index = "frame_index",
      annotation_id = "id", track_id = "track_id", category_id = "category_id",
      category_name = "name", bbox_x = "bbox_1", bbox_y = "bbox_2",
      bbox_width = "bbox_3", bbox_height = "bbox_4", score = "score", area = "area"
    )))

  return(standardized_df)
}

