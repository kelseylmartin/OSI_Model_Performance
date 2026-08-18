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
#' @name OpticsDetections-validity
#' @rdname OpticsDetections-class
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
#' # Example with a temporary KWCOCO JSON file based on Abi's AUV data.
#' kwcoco_json_string <- '{
#'   "info": [{"date_created": "2024-01-01", "description": "Sample from Abi AUV data"}],
#'   "videos": [{"id": 1, "name": "AUV_video_1"}],
#'   "images": [
#'     {"id": 1, "video_id": 1, "frame_index": 1, "file_name": "frame0001.jpg"},
#'     {"id": 2, "video_id": 1, "frame_index": 2, "file_name": "frame0002.jpg"}
#'   ],
#'   "annotations": [
#'     {"id": 1, "image_id": 1, "category_id": 1, "bbox": [10, 20, 30, 40], "score": 0.9},
#'     {"id": 2, "image_id": 2, "category_id": 2, "bbox": [50, 60, 70, 80], "score": 0.95}
#'   ],
#'   "categories": [
#'     {"id": 1, "name": "sea_star"},
#'     {"id": 2, "name": "sea_anemone"}
#'   ]
#' }'
#' temp_json_path <- tempfile(fileext = ".json")
#' writeLines(kwcoco_json_string, temp_json_path)
#'
#' # Ingest the data
#' detections_obj <- read_kwcoco(temp_json_path)
#' print(detections_obj)
#'
#' # Clean up
#' unlink(temp_json_path)
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
#' `OpticsDetections` S4 object. This function can handle two identifier schemas:
#' 'classic' (deriving IDs from the filename and frame number) and 'image_path'
#' (deriving IDs from a full image path in column 2).
#'
#' @param file_path The full path to the VIAME CSV file.
#' @param video_id An optional character string to assign as the video ID.
#'   If `NULL` (default), the ID is derived based on the `id_schema`. This
#'   parameter is ignored when `id_schema` is "image_path".
#' @param id_schema The schema for generating `video_id` and `image_id`.
#'   - `"classic"`: `video_id` is from the filename, `image_id` is a
#'     combination of `video_id` and frame number.
#'   - `"image_path"`: `video_id` and `image_id` are parsed from the explicit
#'     image path provided in the second column of the CSV.
#' @return An `OpticsDetections` object. If the file is empty or unreadable,
#'   the object's `data` slot will be an empty tibble.
#' @export
#' @importFrom readr read_csv cols
#' @importFrom dplyr tibble rename mutate select across any_of
#' @importFrom tools file_path_sans_ext
#' @importFrom stringr str_extract
#' @examples
#' # Example with a temporary VIAME CSV file (classic schema)
#' classic_csv_data <- c(
#'   "1,video1,10,100,100,200,200,1,0.95,\"ringed_seal\",1",
#'   "2,video1,15,150,150,250,250,1,0.90,\"bearded_seal\",1"
#' )
#' temp_classic_path <- tempfile(fileext = ".csv")
#' writeLines(c("# header 1", "# header 2", classic_csv_data), temp_classic_path)
#' detections_classic <- read_viame_csv(temp_classic_path, video_id = "my_video")
#' print(detections_classic)
#' unlink(temp_classic_path)
#'
#' # Example with image path schema
#' image_path_csv_data <- c(
#'"1,C:/data/ice_seals_2025_fl223_L_img.tif,274,535,200,540,205,1,1,animal,1"
#' )
#' temp_image_path <- tempfile(fileext = ".csv")
#' writeLines(c("# header 1", "# header 2", image_path_csv_data), temp_image_path)
#' detections_img <- read_viame_csv(temp_image_path, id_schema = "image_path")
#' print(detections_img)
#' unlink(temp_image_path)
read_viame_csv <- function(file_path, video_id = NULL, id_schema = c("classic", "image_path")) {

  id_schema <- match.arg(id_schema)

  # --- 1. Input Validation ---
  if (!file.exists(file_path)) {
    warning("File does not exist: ", file_path)
    return(OpticsDetections(dplyr::tibble(), file_path, "viame_csv"))
  }

  # --- 2. Read Data ---
  raw_df <- tryCatch({
    suppressWarnings(readr::read_csv(
      file_path,
      skip = 2,
      col_names = FALSE,
      col_types = readr::cols(.default = "c"),
      show_col_types = FALSE
    ))
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


  if (id_schema == "classic") {
    if (is.null(video_id)) {
      video_id <- tools::file_path_sans_ext(basename(file_path))
    }
    standardized_df <- raw_df %>%
      dplyr::mutate(
        video_id = !!video_id,
        image_id = paste0(video_id, "_frame_", .data$UniqFrame)
      )
  } else { # id_schema == "image_path"
    standardized_df <- raw_df %>%
      dplyr::mutate(
        video_id = stringr::str_extract(basename(.data$VidIdent), "ice_seals_\\d{4}_fl\\d+"),
        image_id = basename(.data$VidIdent)
      )
  }

  # --- 4. Common Transformation Logic ---
  standardized_df <- suppressWarnings({
    standardized_df %>%
      dplyr::mutate(
        bbox_width = as.numeric(BR_X) - as.numeric(TL_X),
        bbox_height = as.numeric(BR_Y) - as.numeric(TL_Y)
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
  })

  # Check if NAs were introduced in numeric columns that shouldn't have NAs
  if (any(is.na(standardized_df$bbox_x) | is.na(standardized_df$bbox_y))) {
    warning("NAs introduced by coercion during parsing. Data may be corrupted.")
  }

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

# Internal helper to keep GCS dependency optional and testable.
.list_gcs_objects <- function(bucket_name, prefix) {
  if (!requireNamespace("googleCloudStorageR", quietly = TRUE)) {
    stop(
      "Package 'googleCloudStorageR' is required to scrape GCP bucket URIs. ",
      "Please install it before calling scrape_gcp_uris().",
      call. = FALSE
    )
  }

  googleCloudStorageR::gcs_list_objects(
    bucket = bucket_name,
    prefix = prefix
  )
}

#' Scrape Media URIs from a GCP Bucket Prefix
#'
#' Lists blobs in a Google Cloud Storage bucket under a prefix, filters by
#' file extension, and returns a standardized data frame with parent folder,
#' file name, and full `gs://` URI for each match.
#'
#' @param bucket_name A single bucket name.
#' @param prefix A prefix within the bucket to scan (for example,
#'   `"GFISHER/Video_data/"`).
#' @param extensions Character vector of file extensions to keep (for example,
#'   `c(".mp4", ".avi", ".jpg")`). Case-insensitive.
#' @return A `data.frame` with exactly three columns:
#'   `folder_name`, `file_name`, and `bucket_uri`.
#' @export
scrape_gcp_uris <- function(bucket_name, prefix = "", extensions = c(".mp4", ".avi", ".jpg")) {
  if (!is.character(bucket_name) || length(bucket_name) != 1 || is.na(bucket_name) || bucket_name == "") {
    stop("`bucket_name` must be a single, non-empty character value.", call. = FALSE)
  }
  if (!is.character(prefix) || length(prefix) != 1 || is.na(prefix)) {
    stop("`prefix` must be a single character value.", call. = FALSE)
  }
  if (!is.character(extensions) || length(extensions) == 0 || anyNA(extensions)) {
    stop("`extensions` must be a non-empty character vector.", call. = FALSE)
  }

  normalized_extensions <- tolower(extensions)
  normalized_extensions <- ifelse(
    startsWith(normalized_extensions, "."),
    normalized_extensions,
    paste0(".", normalized_extensions)
  )

  object_listing <- tryCatch(
    .list_gcs_objects(bucket_name = bucket_name, prefix = prefix),
    error = function(e) {
      stop(
        "Failed to list objects from bucket '", bucket_name, "' with prefix '", prefix,
        "'. Check GCP authentication and bucket/prefix access. Original error: ",
        e$message,
        call. = FALSE
      )
    }
  )

  object_names <- character(0)
  if (is.data.frame(object_listing) && "name" %in% names(object_listing)) {
    object_names <- object_listing$name
  } else if (is.list(object_listing) && !is.null(object_listing$name)) {
    object_names <- object_listing$name
  } else if (is.list(object_listing) && !is.null(object_listing$items)) {
    object_names <- vapply(
      object_listing$items,
      function(x) if (!is.null(x$name)) x$name else NA_character_,
      character(1)
    )
  }
  object_names <- object_names[!is.na(object_names)]

  if (length(object_names) == 0) {
    warning(
      "No files found in bucket '", bucket_name, "' under prefix '", prefix, "'.",
      call. = FALSE
    )
    return(data.frame(folder_name = character(), file_name = character(), bucket_uri = character()))
  }

  object_names <- object_names[!grepl("/$", object_names)]
  keep <- tools::file_ext(object_names) != "" &
    paste0(".", tolower(tools::file_ext(object_names))) %in% normalized_extensions
  filtered_names <- object_names[keep]

  if (length(filtered_names) == 0) {
    warning(
      "No files matched requested extensions under prefix '", prefix, "' in bucket '", bucket_name, "'.",
      call. = FALSE
    )
    return(data.frame(folder_name = character(), file_name = character(), bucket_uri = character()))
  }

  relative_paths <- ifelse(startsWith(filtered_names, prefix), substring(filtered_names, nchar(prefix) + 1L), filtered_names)
  file_names <- basename(relative_paths)
  parent_dirs <- dirname(relative_paths)
  folder_names <- ifelse(parent_dirs == "." | parent_dirs == "", NA_character_, basename(parent_dirs))

  data.frame(
    folder_name = folder_names,
    file_name = file_names,
    bucket_uri = paste0("gs://", bucket_name, "/", filtered_names),
    stringsAsFactors = FALSE
  )
}

#' Convert Track Data to KWCOCO
#'
#' Converts track-style detections into a KWCOCO-compatible list, following the
#' track CSV conversion flow used in this repository (track/frame sorting,
#' duplicate track-frame removal, frame-level image creation, species-to-integer
#' category mapping, and bounding box conversion).
#'
#' @param object A source object. Supported inputs are:
#'   - a character path to a VIAME-style track CSV
#'   - a `data.frame` containing track columns
#'   - an `OpticsDetections` object
#' @param ... Additional arguments passed to methods.
#' @return A list with KWCOCO keys (`info`, `videos`, `images`, `annotations`,
#'   `categories`). If `output_path` is supplied, the list is also written to
#'   JSON.
#' @export
#' @rdname convert_track_csv_to_kwcoco
setGeneric("convert_track_csv_to_kwcoco", function(object, ...) {
  standardGeneric("convert_track_csv_to_kwcoco")
})

#' @param output_path Optional file path for writing JSON output.
#' @param video_name Optional video name; defaults to the source filename stem.
#' @param video_id Integer video ID to include in KWCOCO videos.
#' @param col_mapping Named list mapping canonical fields to column names.
#'   Required mapping keys: `frame`, `track_id`, `tl_x`, `tl_y`, `br_x`,
#'   `br_y`, `species_name`. Optional: `score`.
#' @param info Optional KWCOCO `info` object. If `NULL`, a default info list is
#'   generated.
#'
#' @rdname convert_track_csv_to_kwcoco
#' @export
#' @importFrom jsonlite write_json
setMethod("convert_track_csv_to_kwcoco", "character",
          function(object,
                   output_path = NULL,
                   video_name = NULL,
                   video_id = 1L,
                   col_mapping = list(
                     frame = "UniqFrame",
                     track_id = "TrackID",
                     tl_x = "TL_X",
                     tl_y = "TL_Y",
                     br_x = "BR_X",
                     br_y = "BR_Y",
                     score = "DetLen_Conf",
                     species_name = "SP"
                   ),
                   info = NULL) {
            if (!file.exists(object)) {
              stop("File does not exist: ", object)
            }

            raw_df <- tryCatch({
              suppressWarnings(readr::read_csv(
                object,
                skip = 2,
                col_names = FALSE,
                col_select = c(1:11),
                show_col_types = FALSE
              ))
            }, error = function(e) {
              stop("Failed to read track CSV: ", object, " - Error: ", e$message)
            })

            if (nrow(raw_df) == 0) {
              stop("Track CSV is empty after reading: ", basename(object))
            }

            raw_df <- raw_df[rowSums(is.na(raw_df)) != ncol(raw_df), , drop = FALSE]
            viame_names <- c(
              "TrackID", "VidIdent", "UniqFrame", "TL_X", "TL_Y", "BR_X", "BR_Y",
              "DetLen_Conf", "Tar_Len", "SP", "CP"
            )
            names(raw_df)[1:min(ncol(raw_df), length(viame_names))] <- viame_names[1:min(ncol(raw_df), length(viame_names))]

            if (is.null(video_name)) {
              video_name <- tools::file_path_sans_ext(basename(object))
            }

            convert_track_csv_to_kwcoco(
              object = as.data.frame(raw_df),
              output_path = output_path,
              video_name = video_name,
              video_id = video_id,
              col_mapping = col_mapping,
              info = info
            )
          })

#' @rdname convert_track_csv_to_kwcoco
#' @export
setMethod("convert_track_csv_to_kwcoco", "OpticsDetections",
          function(object,
                   output_path = NULL,
                   video_name = NULL,
                   video_id = 1L,
                   info = NULL) {
            detections_df <- object@data
            required_cols <- c("frame_index", "annotation_id", "category_name",
                               "bbox_x", "bbox_y", "bbox_width", "bbox_height")
            missing_cols <- setdiff(required_cols, names(detections_df))
            if (length(missing_cols) > 0) {
              stop("OpticsDetections@data is missing required columns: ",
                   paste(missing_cols, collapse = ", "))
            }

            track_ids <- if ("track_id" %in% names(detections_df)) detections_df$track_id else detections_df$annotation_id
            score_vals <- if ("score" %in% names(detections_df)) detections_df$score else NA_real_

            track_df <- dplyr::tibble(
              TrackID = track_ids,
              UniqFrame = detections_df$frame_index,
              TL_X = detections_df$bbox_x,
              TL_Y = detections_df$bbox_y,
              BR_X = detections_df$bbox_x + detections_df$bbox_width,
              BR_Y = detections_df$bbox_y + detections_df$bbox_height,
              SP = detections_df$category_name,
              DetLen_Conf = score_vals
            )

            if (is.null(video_name) && "video_id" %in% names(detections_df) && nrow(detections_df) > 0) {
              video_name <- as.character(detections_df$video_id[[1]])
            }
            if (is.null(video_name)) {
              video_name <- "video_1"
            }

            convert_track_csv_to_kwcoco(
              object = as.data.frame(track_df),
              output_path = output_path,
              video_name = video_name,
              video_id = video_id,
              col_mapping = list(
                frame = "UniqFrame",
                track_id = "TrackID",
                tl_x = "TL_X",
                tl_y = "TL_Y",
                br_x = "BR_X",
                br_y = "BR_Y",
                score = "DetLen_Conf",
                species_name = "SP"
              ),
              info = info
            )
          })

#' @rdname convert_track_csv_to_kwcoco
#' @export
setMethod("convert_track_csv_to_kwcoco", "data.frame",
          function(object,
                   output_path = NULL,
                   video_name = "video_1",
                   video_id = 1L,
                   col_mapping = list(
                     frame = "UniqFrame",
                     track_id = "TrackID",
                     tl_x = "TL_X",
                     tl_y = "TL_Y",
                     br_x = "BR_X",
                     br_y = "BR_Y",
                     score = "DetLen_Conf",
                     species_name = "SP"
                   ),
                   info = NULL) {
            required_mapping <- c("frame", "track_id", "tl_x", "tl_y", "br_x", "br_y", "species_name")
            missing_mapping <- setdiff(required_mapping, names(col_mapping))
            if (length(missing_mapping) > 0) {
              stop("col_mapping is missing required keys: ", paste(missing_mapping, collapse = ", "))
            }

            required_cols <- unname(unlist(col_mapping[required_mapping]))
            missing_cols <- setdiff(required_cols, names(object))
            if (length(missing_cols) > 0) {
              stop("Input data frame is missing required columns: ", paste(missing_cols, collapse = ", "))
            }

            if (nrow(object) == 0) {
              stop("Input data frame is empty; no annotations to convert.")
            }

            frame_vals <- suppressWarnings(as.integer(object[[col_mapping$frame]]))
            track_vals <- suppressWarnings(as.integer(object[[col_mapping$track_id]]))
            tl_x_vals <- suppressWarnings(as.numeric(object[[col_mapping$tl_x]]))
            tl_y_vals <- suppressWarnings(as.numeric(object[[col_mapping$tl_y]]))
            br_x_vals <- suppressWarnings(as.numeric(object[[col_mapping$br_x]]))
            br_y_vals <- suppressWarnings(as.numeric(object[[col_mapping$br_y]]))

            invalid_rows <- is.na(frame_vals) | is.na(track_vals) |
              is.na(tl_x_vals) | is.na(tl_y_vals) | is.na(br_x_vals) | is.na(br_y_vals)
            if (any(invalid_rows)) {
              warning(sum(invalid_rows), " rows dropped due to invalid frame/track/bounding box values.")
              object <- object[!invalid_rows, , drop = FALSE]
              frame_vals <- frame_vals[!invalid_rows]
              track_vals <- track_vals[!invalid_rows]
              tl_x_vals <- tl_x_vals[!invalid_rows]
              tl_y_vals <- tl_y_vals[!invalid_rows]
              br_x_vals <- br_x_vals[!invalid_rows]
              br_y_vals <- br_y_vals[!invalid_rows]
            }

            bbox_w <- br_x_vals - tl_x_vals
            bbox_h <- br_y_vals - tl_y_vals
            invalid_bbox <- bbox_w <= 0 | bbox_h <= 0
            if (any(invalid_bbox)) {
              warning(sum(invalid_bbox), " rows dropped due to non-positive bbox width/height.")
              keep_rows <- !invalid_bbox
              object <- object[keep_rows, , drop = FALSE]
              frame_vals <- frame_vals[keep_rows]
              track_vals <- track_vals[keep_rows]
              tl_x_vals <- tl_x_vals[keep_rows]
              tl_y_vals <- tl_y_vals[keep_rows]
              br_x_vals <- br_x_vals[keep_rows]
              br_y_vals <- br_y_vals[keep_rows]
              bbox_w <- bbox_w[keep_rows]
              bbox_h <- bbox_h[keep_rows]
            }

            if (nrow(object) == 0) {
              stop("No valid rows remain after validation.")
            }

            order_idx <- order(track_vals, frame_vals)
            object <- object[order_idx, , drop = FALSE]
            frame_vals <- frame_vals[order_idx]
            track_vals <- track_vals[order_idx]
            tl_x_vals <- tl_x_vals[order_idx]
            tl_y_vals <- tl_y_vals[order_idx]
            br_x_vals <- br_x_vals[order_idx]
            br_y_vals <- br_y_vals[order_idx]

            dedup_idx <- !duplicated(data.frame(track = track_vals, frame = frame_vals))
            if (any(!dedup_idx)) {
              object <- object[dedup_idx, , drop = FALSE]
              frame_vals <- frame_vals[dedup_idx]
              track_vals <- track_vals[dedup_idx]
              tl_x_vals <- tl_x_vals[dedup_idx]
              tl_y_vals <- tl_y_vals[dedup_idx]
              br_x_vals <- br_x_vals[dedup_idx]
              br_y_vals <- br_y_vals[dedup_idx]
            }

            species_vals <- as.character(object[[col_mapping$species_name]])
            species_vals[is.na(species_vals) | species_vals == ""] <- "Unknown"
            unique_species <- unique(species_vals)
            species_to_int_map <- setNames(seq_along(unique_species), unique_species)

            unique_frames <- unique(frame_vals)
            image_ids <- seq.int(0L, length(unique_frames) - 1L)
            frame_to_image <- stats::setNames(as.list(image_ids), as.character(unique_frames))

            images <- lapply(seq_along(unique_frames), function(i) {
              frame_i <- unique_frames[[i]]
              list(
                id = image_ids[[i]],
                file_name = sprintf("frame_%06d.jpg", frame_i),
                frame_index = frame_i
              )
            })

            score_values <- NULL
            if (!is.null(col_mapping$score) && col_mapping$score %in% names(object)) {
              score_values <- suppressWarnings(as.numeric(object[[col_mapping$score]]))
            }

            annotations <- lapply(seq_len(nrow(object)), function(i) {
              bbox <- floor(c(
                tl_x_vals[[i]],
                tl_y_vals[[i]],
                br_x_vals[[i]] - tl_x_vals[[i]],
                br_y_vals[[i]] - tl_y_vals[[i]]
              ))
              ann <- list(
                id = i,
                image_id = frame_to_image[[as.character(frame_vals[[i]])]],
                category_id = species_to_int_map[[species_vals[[i]]]],
                track_id = track_vals[[i]],
                bbox = bbox,
                iscrowd = 0,
                area = bbox[[3]] * bbox[[4]]
              )
              if (!is.null(score_values)) {
                ann$score <- score_values[[i]]
              }
              ann
            })

            categories <- lapply(names(species_to_int_map), function(s_name) {
              list(id = species_to_int_map[[s_name]], name = s_name, keypoints = c("head", "tail"))
            })

            if (is.null(info)) {
              info <- list(
                description = "Optics track conversion",
                version = "1.0",
                year = as.integer(format(Sys.Date(), "%Y")),
                date_created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                dive_extensions = c("dive_detection_attributes", "dive_track_attributes")
              )
            }

            kwcoco_data <- list(
              info = info,
              videos = list(list(id = as.integer(video_id), name = as.character(video_name))),
              images = images,
              annotations = annotations,
              categories = categories
            )

            if (!is.null(output_path)) {
              jsonlite::write_json(kwcoco_data, path = output_path, auto_unbox = TRUE, pretty = TRUE)
            }

            kwcoco_data
          })

#' Read and Transform a Wide-Format MaxN CSV File
#'
#' Ingests a CSV file containing species counts (like MaxN) in a wide format,
#' where columns represent species and rows represent observations, and
#' transforms it into a long-format tibble suitable for alignment.
#'
#' @param file_path The full path to the wide-format CSV file.
#' @param ... Additional arguments passed to methods.
#' @return A `tibble` in long format with columns for the video identifier,
#'   `category_name`, and `truth_count`.
#' @export
#' @rdname read_wide_maxn
setGeneric("read_wide_maxn", function(file_path, ...) {
  standardGeneric("read_wide_maxn")
})

#' @param video_id_col The unquoted name of the column to be used as the
#'   video/observation identifier. Defaults to `REFERENCE`.
#' @param pivot_cols A tidyselect expression for the columns to pivot from wide
#'   to long format. Defaults to all columns except `LAB` and `REFERENCE`.
#'
#' @rdname read_wide_maxn
#' @export
#' @importFrom utils read.csv
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr rename filter select any_of
setMethod("read_wide_maxn", "character",
  function(file_path, video_id_col = REFERENCE, pivot_cols = -dplyr::any_of(c("LAB", "REFERENCE"))) {

    if (!file.exists(file_path)) {
      stop("File does not exist: ", file_path)
    }

    data <- utils::read.csv(file_path, check.names = FALSE)

    long_data <- data %>%
      tidyr::pivot_longer(
        cols = {{ pivot_cols }},
        names_to = "category_name",
        values_to = "truth_count"
      ) %>%
      dplyr::rename(video_id = {{ video_id_col }}) %>%
      dplyr::filter(.data$truth_count > 0) %>%
      dplyr::select("video_id", "category_name", "truth_count")

    # Ensure video_id is character for joining
    long_data$video_id <- as.character(long_data$video_id)

    return(long_data)
  })
