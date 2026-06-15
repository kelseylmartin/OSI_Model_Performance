# Required for S4 class definition
#' @import methods
NULL

#' An S4 class to represent standardized detection data.
#'
#' The `OpticsDetections` class serves as a container for object detection data
#' ingested from various sources, such as KWCOCO JSON or VIAME CSV files. It
#' standardizes the data into a single `tibble` format, ensuring consistency
#' for downstream analysis.
#'
#' @slot data A `tibble` where each row is a detection, containing standardized
#'   columns like `video_id`, `image_id`, `category_name`, bounding box
#'   coordinates, and `score`.
#' @slot source_file A character string storing the path of the file from which
#'   the data was ingested.
#' @slot ingest_format A character string indicating the original format of the
#'   data (e.g., "kwcoco", "viame_csv").
#'
#' @exportClass OpticsDetections
#' @rdname OpticsDetections-class
setClass("OpticsDetections",
         slots = c(
           data = "data.frame",
           source_file = "character",
           ingest_format = "character"
         ),
         prototype = list(
           data = dplyr::tibble(),
           source_file = NA_character_,
           ingest_format = NA_character_
         )
)

#' Validity check for OpticsDetections objects
#'
#' @importFrom dplyr all_of
setValidity("OpticsDetections", function(object) {
  errors <- character()
  
  # 1. Check if the 'data' slot is a data.frame
  if (!is.data.frame(object@data)) {
    errors <- c(errors, "'data' slot must be a data.frame or tibble.")
  }
  
  # 2. Check for required columns if data is not empty
  if (nrow(object@data) > 0) {
    required_cols <- c("video_id", "image_id", "frame_index", "annotation_id",
                       "category_name", "bbox_x", "bbox_y", "bbox_width",
                       "bbox_height", "score")
    
    missing_cols <- setdiff(required_cols, names(object@data))
    
    if (length(missing_cols) > 0) {
      msg <- paste0("The 'data' slot is missing required columns: ",
                    paste(missing_cols, collapse = ", "))
      errors <- c(errors, msg)
    }
  }
  
  # 3. Check that source_file and ingest_format are single character strings
  if (length(object@source_file) != 1 || !is.character(object@source_file)) {
    errors <- c(errors, "'source_file' must be a single character string.")
  }
  
  if (length(object@ingest_format) != 1 || !is.character(object@ingest_format)) {
    errors <- c(errors, "'ingest_format' must be a single character string.")
  }
  
  if (length(errors) == 0) TRUE else errors
})


#' Constructor for OpticsDetections objects
#'
#' A user-friendly constructor to create instances of the `OpticsDetections`
#' class. It wraps the standard `new()` method and provides a more intuitive
#' interface.
#'
#' @param data A `data.frame` or `tibble` of detection data.
#' @param source_file The path to the source data file.
#' @param ingest_format The format of the ingested data (e.g., "kwcoco").
#' @return An `OpticsDetections` object.
#' @export
OpticsDetections <- function(data, source_file, ingest_format) {
  new("OpticsDetections",
      data = as.data.frame(data),
      source_file = source_file,
      ingest_format = ingest_format)
}


#' Read and Flatten a KWCOCO JSON file
#'
#' Ingests a KWCOCO-formatted JSON file and wraps the standardized data in an
#' `OpticsDetections` S4 object.
#'
#' @param file_path The full path to the KWCOCO JSON file.
#' @return An `OpticsDetections` object containing the flattened annotation
#'   data. If the file is invalid or empty, the object's `data` slot will be
#'   an empty tibble.
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr tibble inner_join select any_of bind_rows bind_cols rename
#' @importFrom tidyr hoist
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
#'   detections_obj <- read_kwcoco(json_path)
#'   print(detections_obj@data)
#'
#'   # Clean up
#'   unlink(json_path)
#' }
read_kwcoco <- function(file_path) {

  # --- 1. Input Validation and Reading ---
  if (!file.exists(file_path)) {
    warning("File does not exist: ", file_path)
    return(OpticsDetections(dplyr::tibble(), file_path, "kwcoco"))
  }

  kwcoco_data <- tryCatch({
    jsonlite::fromJSON(file_path, simplifyDataFrame = FALSE)
  }, error = function(e) {
    warning("Failed to parse JSON file: ", file_path, " - Error: ", e$message)
    return(NULL)
  })

  if (is.null(kwcoco_data) || length(kwcoco_data$annotations) == 0) {
    warning("KWCOCO file is empty or contains no annotations: ", basename(file_path))
    return(OpticsDetections(dplyr::tibble(), file_path, "kwcoco"))
  }

  # --- 2. Create Lookup Tables ---
  images_df <- dplyr::bind_rows(kwcoco_data$images)
  categories_df <- dplyr::bind_rows(kwcoco_data$categories)
  videos_df <- dplyr::bind_rows(kwcoco_data$videos)

  # --- 3. Process Annotations ---
  annotations_df <- dplyr::bind_rows(lapply(kwcoco_data$annotations, function(x) x[names(x) != "bbox"]))
  bbox_list <- lapply(kwcoco_data$annotations, `[[`, "bbox")
  bbox_df <- as.data.frame(do.call(rbind, bbox_list))
  names(bbox_df) <- c("bbox_1", "bbox_2", "bbox_3", "bbox_4")
  annotations_df <- dplyr::bind_cols(annotations_df, bbox_df)

  # --- 4. Join and Standardize ---
  # Add frame_index if it's missing from images_df
  if (!"frame_index" %in% names(images_df)) {
    images_df$frame_index <- NA_integer_
  }

  flat_df <- suppressMessages({
    annotations_df %>%
      dplyr::inner_join(images_df, by = c("image_id" = "id"), suffix = c("_ann", "_img")) %>%
      dplyr::inner_join(categories_df, by = c("category_id" = "id"), suffix = c("", "_cat")) %>%
      dplyr::inner_join(videos_df, by = c("video_id" = "id"), suffix = c("_img", "_vid"))
  })

  standardized_df <- flat_df %>%
    dplyr::select(-image_id, -video_id) %>%
    dplyr::rename(
      annotation_id = "id",
      category_name = "name_img",
      video_id = "name_vid",
      image_id = "file_name",
      bbox_x = "bbox_1",
      bbox_y = "bbox_2",
      bbox_width = "bbox_3",
      bbox_height = "bbox_4"
    ) %>%
    dplyr::select(dplyr::any_of(c(
      "video_id", "image_id", "frame_index", "annotation_id", "track_id",
      "category_id", "category_name", "bbox_x", "bbox_y",
      "bbox_width", "bbox_height", "score", "area"
    )))

  # Ensure all required columns exist, adding them as NA if necessary
  required_cols <- c("video_id", "image_id", "frame_index", "annotation_id",
                     "category_name", "bbox_x", "bbox_y", "bbox_width",
                     "bbox_height", "score")
  for (col in required_cols) {
    if (!col %in% names(standardized_df)) {
      standardized_df[[col]] <- NA
    }
  }


  return(OpticsDetections(standardized_df, file_path, "kwcoco"))
}


#' Read and Standardize a VIAME-style CSV File
#'
#' Ingests a CSV file from VIAME and wraps the standardized data in an
#' `OpticsDetections` S4 object.
#'
#' @param file_path The full path to the VIAME CSV file.
#' @param video_id An optional character string to assign as the video ID.
#'   If `NULL`, the ID is derived from the input file's name.
#' @return An `OpticsDetections` object. If the file is empty or unreadable,
#'   the object's `data` slot will be an empty tibble.
#' @export
#' @importFrom readr read_csv cols
#' @importFrom dplyr tibble rename mutate select across
#' @importFrom tools file_path_sans_ext
read_viame_csv <- function(file_path, video_id = NULL) {

  # --- 1. Input Validation ---
  if (!file.exists(file_path)) {
    warning("File does not exist: ", file_path)
    return(OpticsDetections(dplyr::tibble(), file_path, "viame_csv"))
  }

  # --- 2. Read Data ---
  raw_df <- tryCatch({
    readr::read_csv(
      file_path,
      skip = 2,
      col_names = FALSE,
      col_types = readr::cols(.default = "c"),
      show_col_types = FALSE
    )
  }, error = function(e) {
    warning("Failed to read CSV file: ", file_path, " - Error: ", e$message)
    return(NULL)
  })

  if (is.null(raw_df) || nrow(raw_df) == 0) {
    warning("VIAME CSV file is empty or could not be read: ", basename(file_path))
    return(OpticsDetections(dplyr::tibble(), file_path, "viame_csv"))
  }

  # --- 3. Assign Column Names and Standardize ---
  viame_names <- c(
    "TrackID", "VidIdent", "UniqFrame", "TL_X", "TL_Y", "BR_X", "BR_Y",
    "DetLen_Conf", "Tar_Len", "SP", "CP"
  )
  names(raw_df)[1:min(ncol(raw_df), length(viame_names))] <- viame_names[1:min(ncol(raw_df), length(viame_names))]

  if (is.null(video_id)) {
    video_id <- tools::file_path_sans_ext(basename(file_path))
  }

  standardized_df <- raw_df %>%
    dplyr::mutate(
      video_id = !!video_id,
      image_id = paste0(video_id, "_frame_", .data$UniqFrame),
      bbox_width = as.numeric(.data$BR_X) - as.numeric(.data$TL_X),
      bbox_height = as.numeric(.data$BR_Y) - as.numeric(.data$TL_Y)
    ) %>%
    dplyr::rename(
      annotation_id = "TrackID",
      frame_index = "UniqFrame",
      category_name = "SP",
      score = "DetLen_Conf",
      bbox_x = "TL_X",
      bbox_y = "TL_Y"
    ) %>%
    dplyr::mutate(dplyr::across(
      c(frame_index, bbox_x, bbox_y, bbox_width, bbox_height, score),
      ~as.numeric(as.character(.))
    )) %>%
    dplyr::select(
      dplyr::any_of(c(
        "video_id", "image_id", "frame_index", "annotation_id", "category_name",
        "bbox_x", "bbox_y", "bbox_width", "bbox_height", "score"
      ))
    )

  # Ensure all required columns exist, adding them as NA if necessary
  required_cols <- c("video_id", "image_id", "frame_index", "annotation_id",
                     "category_name", "bbox_x", "bbox_y", "bbox_width",
                     "bbox_height", "score")
  for (col in required_cols) {
    if (!col %in% names(standardized_df)) {
      standardized_df[[col]] <- NA
    }
  }

  return(OpticsDetections(standardized_df, file_path, "viame_csv"))
}
