# Install and load necessary libraries
# install.packages(c("readr", "jsonlite", "googleCloudStorageR"))
library(readr)
library(jsonlite)
library(googleCloudStorageR)

# --- Optional: Customize dataset info ---
INFO <- list(
  description = "My Custom Tracking Dataset",
  url = "",
  version = "1.0",
  year = as.integer(format(Sys.Date(), "%Y")),
  contributor = "Your Name",
  date_created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  dive_extensions = c("dive_detection_attributes", "dive_track_attributes")
)

#' Pre-processes and reads a raw tracking CSV.
#'
#' This function is tailored to read the specific raw CSV format which has no
#' headers and two initial junk rows. It applies the correct column names and
#' performs necessary data cleaning.
#'
#' @param flnm The file path of the CSV to read.
#' @return A data frame with corrected headers and types, or NULL on error.
preprocess_and_read_csv <- function(flnm) {
  # --- Cloud vs Local Read ---
  # Check if the path is a GCS path (gs://bucket/object)
  is_gcs_path <- grepl("^gs://", flnm)
  local_flnm_to_read <- flnm
  
  if (is_gcs_path) {
    cat(sprintf(" (cloud) -> %s\n", flnm))
    # Download GCS object to a temporary file for reading
    local_flnm_to_read <- tempfile(fileext = ".csv")
    on.exit(unlink(local_flnm_to_read)) # Ensure temp file is deleted
    gcs_obj_meta <- gcs_parse_url(flnm)
    gcs_get_object(gcs_obj_meta$object, bucket = gcs_obj_meta$bucket, saveToDisk = local_flnm_to_read)
  }
  # --- End Cloud vs Local ---
  
  tryCatch({
    # Read the first 11 columns, skipping 2 header rows, with no column names
    df <- read_csv(local_flnm_to_read, skip = 2, col_names = FALSE, col_select = c(1:11), show_col_types = FALSE)
    
    if (nrow(df) == 0) return(NULL)
    
    # Replicate filename processing to create Deployment ID
    deployment_name <- basename(flnm)
    deployment_name <- gsub("\\.csv$", "", deployment_name)
    deployment_name <- gsub("_\\d+\\.\\d+_(tracks|detections)$|_cam.*_(tracks|detections)$|_(tracks|detections)_cam.*$| cam.*_(tracks|detections)$|_(tracks|detections)$","", deployment_name)
    deployment_name <- gsub("_|-", "", deployment_name)
    
    # Year-specific fixes based on filename patterns
    if (grepl("^19[WE]_", deployment_name)) {
      deployment_name <- gsub("^19W_", "2019W", deployment_name)
      deployment_name <- gsub("^19E_", "2019E", deployment_name)
    } else if (grepl("^(?:[A-Za-z]+_)?(19|20)\\d{2}", flnm) || grepl("(19|20)\\d{2}", flnm)) { 
      # Attempting to loosely guess if it's year data from the path
      year_match <- regmatches(flnm, regexpr("(19|20)\\d{2}", flnm))
      if (length(year_match) > 0) {
        year_str <- year_match[[1]]
        if (grepl("^[A-Za-z]", deployment_name) && !grepl("^(19|20)\\d{2}", deployment_name)) {
          deployment_name <- paste0(year_str, "-", deployment_name)
        }
      }
    }
    
    df$X12 <- deployment_name
    
    # Assign all column names
    col.names <- c("TrackID", "VidIdent", "UniqFrame", "TL_X", "TL_Y", "BR_X", "BR_Y", "DetLen_Conf", "Tar_Len", "SP", "CP", "Deployment")
    names(df) <- col.names
    
    return(df)
  }, error = function(e) {
    print(paste("Error processing file: ", flnm, " - ", e$message))
    return(NULL)
  })
}


#' Converts a single tracking CSV file to the KWCOCO JSON format.
#'
#' @param csv_path Path to the input CSV file. The CSV should have columns:
#'   'frame', 'track_id', 'bb_left', 'bb_top', 'bb_width', 'bb_height', 'class_id'.
#' @param col_mapping A named list that maps standard names (e.g., 'frame', 'track_id', 'tl_x')
#'   to the actual column names in the CSV. Supports 'tl_x', 'tl_y', 'br_x', 'br_y' for bounding boxes.
#' @param default_category_id A default category ID to use if no category mapping is provided.
#' @param video_info A named list containing video metadata:
#'   list(id = integer, name = character, width = integer, height = integer).
#' @param output_path Path to save the output KWCOCO JSON file.
#'
convert_track_csv_to_kwcoco_r <- function(csv_path,
                                          video_info,
                                          output_path,
                                          col_mapping,
                                          default_category_id = 1) {
  
  cat(sprintf("Processing video: %s from %s\n", video_info$name, csv_path))
  
  # 1. Initialize the master KWCOCO list
  kwcoco_data <- list(
    info = INFO,
    images = list(),
    annotations = list()
  )
  
  # 2. Pre-process and read the raw CSV data (now supports local and gs:// paths)
  df <- preprocess_and_read_csv(csv_path)
  
  if (is.null(df)) { return(invisible(NULL)) } # Skip if file was empty or failed to read
  
  # Sort dataframe by track_id (numerically) and frame to ensure annotations match DIVE sort order
  df <- df[order(as.numeric(df[[col_mapping$track_id]]), as.numeric(df[[col_mapping$frame]])), ]
  
  # Remove duplicate track/frame combinations (DIVE expects exactly one annotation per track per frame)
  duplicate_rows <- duplicated(df[c(col_mapping$track_id, col_mapping$frame)])
  if (any(duplicate_rows)) {
    df <- df[!duplicate_rows, ]
  }
  
  # 3. Process data frame and populate KWCOCO structure
  annotation_id_counter <- 1
  image_id_counter <- 0
  
  # Use a named list as a hash map to track processed frames
  processed_frames <- list() 
  all_species <- c()
  
  for (i in 1:nrow(df)) {
    row <- df[i, ]
    
    frame_number <- as.integer(row[[col_mapping$frame]])
    frame_index <- frame_number
    track_id <- as.integer(row[[col_mapping$track_id]])
    
    # Bounding box calculation from top-left and bottom-right coordinates
    tl_x <- as.numeric(row[[col_mapping$tl_x]])
    tl_y <- as.numeric(row[[col_mapping$tl_y]])
    br_x <- as.numeric(row[[col_mapping$br_x]])
    br_y <- as.numeric(row[[col_mapping$br_y]])
    bbox <- floor(c(tl_x, tl_y, br_x - tl_x, br_y - tl_y))
    
    # Get species name for category mapping
    species_name <- as.character(row[[col_mapping$species_name]])
    all_species <- c(all_species, species_name)
    category_id <- species_name
    
    # Convert frame_number to character for use as a list name
    frame_key <- as.character(frame_number)
    
    # If it's a new frame, create a new image entry
    if (is.null(processed_frames[[frame_key]])) {
      current_image_id <- image_id_counter
      processed_frames[[frame_key]] <- current_image_id
      
      image_entry <- list(
        id = current_image_id,
        file_name = sprintf("frame_%06d.jpg", frame_index),
        frame_index = frame_index
      )
      
      kwcoco_data$images <- append(kwcoco_data$images, list(image_entry))
      image_id_counter <- image_id_counter + 1
    } else {
      current_image_id <- processed_frames[[frame_key]]
    }
    
    # Create the annotation entry
    annotation_entry <- list(
      id = annotation_id_counter,
      image_id = current_image_id,
      category_id = category_id,
      track_id = track_id, # Already top-level
      bbox = bbox,
      iscrowd = 0,
      area = bbox[3] * bbox[4]
    )
    
    # Add score if mapped
    if (!is.null(col_mapping$score) && col_mapping$score %in% names(row)) {
      annotation_entry$score <- as.numeric(row[[col_mapping$score]])
    }
    
    kwcoco_data$annotations <- append(kwcoco_data$annotations, list(annotation_entry))
    annotation_id_counter <- annotation_id_counter + 1
  }
  
  # Create integer mapping for species and generate final categories list
  unique_species <- unique(all_species)
  species_to_int_map <- setNames(seq_along(unique_species), unique_species)
  
  kwcoco_data$categories <- lapply(names(species_to_int_map), function(s_name) {
    list(id = species_to_int_map[[s_name]], name = s_name, keypoints = c("head", "tail"))
  })
  
  # Update annotations with integer category IDs
  kwcoco_data$annotations <- lapply(kwcoco_data$annotations, function(ann) {
    ann$category_id <- species_to_int_map[[ann$category_id]]
    return(ann)
  })
  
  # 4. Write the final list to a JSON file
  cat(sprintf("Conversion complete. Writing to %s\n", output_path))
  write_json(
    kwcoco_data, 
    path = output_path, 
    auto_unbox = TRUE, # Ensures single values are not written as arrays
    pretty = TRUE      # For human-readable output
  )
}

#' Converts a directory of tracking CSV files into a single KWCOCO JSON file.
#'
#' @param csv_inputs A named list where names are video names (e.g., "video_01")
#'   and values are paths to the corresponding CSV files.
#' @param col_mapping A named list that maps standard names (e.g., 'frame', 'track_id', 'tl_x')
#'   to the actual column names in the CSV. Supports 'tl_x', 'tl_y', 'br_x', 'br_y' for bounding boxes.
#' @param default_category_id A default category ID to use if no category mapping is provided.
#' @param video_metadata_list A named list of video metadata, where names
#'   match the video names in `csv_inputs`.
#' @param output_path Path to save the single output KWCOCO JSON file.
#'
convert_batch_to_kwcoco_r <- function(csv_inputs, video_metadata_list, output_path, col_mapping, default_category_id = 1) {
  
  cat("Starting batch conversion...\n")
  
  # 1. Initialize the master KWCOCO list for the entire batch
  kwcoco_data <- list(
    info = INFO,
    images = list(),
    annotations = list()
  )
  
  # --- Global counters for unique IDs across all files ---
  master_annotation_id <- 1
  master_image_id <- 0
  all_species <- c()
  
  # 2. Iterate over each video and its corresponding CSV
  for (video_name in names(csv_inputs)) {
    csv_path <- csv_inputs[[video_name]]
    video_info <- video_metadata_list[[video_name]]
    
    if (is.null(video_info)) {
      warning(sprintf("Warning: No metadata found for video '%s'. Skipping.", video_name))
      next
    }
    
    cat(sprintf("Processing video: %s from %s\n", video_name, csv_path))
    
    # Pre-process and read the raw CSV data (now supports local and gs:// paths)
    cat(sprintf("Reading %s", csv_path))
    df <- preprocess_and_read_csv(csv_path)
    
    if (is.null(df)) { next } # Skip if file was empty or failed to read
    
    # Sort dataframe by track_id (numerically) and frame to ensure annotations match DIVE sort order
    df <- df[order(as.numeric(df[[col_mapping$track_id]]), as.numeric(df[[col_mapping$frame]])), ]
    
    # Remove duplicate track/frame combinations (DIVE expects exactly one annotation per track per frame)
    duplicate_rows <- duplicated(df[c(col_mapping$track_id, col_mapping$frame)])
    if (any(duplicate_rows)) {
      df <- df[!duplicate_rows, ]
    }
    
    processed_frames <- list() # Tracks frames *within the current video*
    
    # 3. Process each row of the current CSV
    for (i in 1:nrow(df)) {
      row <- df[i, ]
      frame_number <- as.integer(row[[col_mapping$frame]])
      frame_index <- frame_number
      frame_key <- as.character(frame_number)
      
      # If it's a new frame for this video, create an image entry
      if (is.null(processed_frames[[frame_key]])) {
        current_image_id <- master_image_id
        processed_frames[[frame_key]] <- current_image_id
        
        image_entry <- list(
          id = current_image_id,
          file_name = sprintf("frame_%06d.jpg", frame_index),
          frame_index = frame_index
        )
        kwcoco_data$images <- append(kwcoco_data$images, list(image_entry))
        master_image_id <- master_image_id + 1
      }
      
      # Bounding box calculation
      tl_x <- as.numeric(row[[col_mapping$tl_x]])
      tl_y <- as.numeric(row[[col_mapping$tl_y]])
      br_x <- as.numeric(row[[col_mapping$br_x]])
      br_y <- as.numeric(row[[col_mapping$br_y]])
      bbox <- floor(c(tl_x, tl_y, br_x - tl_x, br_y - tl_y))
      
      # Get species name for category mapping
      species_name <- as.character(row[[col_mapping$species_name]])
      all_species <- c(all_species, species_name)
      category_id <- species_name
      
      # Create the base annotation entry
      annotation_entry <- list(
        id = master_annotation_id,
        image_id = processed_frames[[frame_key]],
        category_id = category_id,
        track_id = as.integer(row[[col_mapping$track_id]]),
        bbox = bbox,
        iscrowd = 0,
        area = bbox[3] * bbox[4]
      )
      
      # Add score if mapped
      if (!is.null(col_mapping$score) && col_mapping$score %in% names(row)) {
        annotation_entry$score <- as.numeric(row[[col_mapping$score]])
      }
      
      kwcoco_data$annotations <- append(kwcoco_data$annotations, list(annotation_entry))
      master_annotation_id <- master_annotation_id + 1
    }
  }
  
  # Create integer mapping for species and generate final categories list
  unique_species <- unique(all_species)
  species_to_int_map <- setNames(seq_along(unique_species), unique_species)
  
  kwcoco_data$categories <- lapply(names(species_to_int_map), function(s_name) {
    list(id = species_to_int_map[[s_name]], name = s_name, keypoints = c("head", "tail"))
  })
  
  # Update annotations with integer category IDs
  kwcoco_data$annotations <- lapply(kwcoco_data$annotations, function(ann) {
    ann$category_id <- species_to_int_map[[ann$category_id]]
    return(ann)
  })
  
  # 4. Write the final combined dictionary to a single JSON file
  cat(sprintf("\nBatch conversion complete. Writing combined data to %s\n", output_path))
  write_json(kwcoco_data, path = output_path, auto_unbox = TRUE, pretty = TRUE)
}

#' Converts all CSV files in a folder to a single KWCOCO JSON file. 
#' 
#' This function scans a directory for .csv files, automatically generates 
#' video metadata, and then calls the batch converter to produce a single
#' KWCOCO file in the same directory.
#'
#' @param folder_path Path to the directory containing the CSV files.
#' @param output_filename The name for the output JSON file.
#' @param col_mapping A named list that maps standard KWCOCO concepts to CSV column names.
#' @param default_category_id A default category ID to use if no category mapping is provided.
#' @param default_video_width Default width to use for video metadata. This is a placeholder
#'   as video dimensions are not typically in tracking CSVs.
#' @param default_video_height Default height to use for video metadata.
#'
convert_folder_to_kwcoco_r <- function(folder_path,
                                       output_filename = "output_kwcoco.json",
                                       col_mapping,
                                       default_category_id = 1,
                                       default_video_width = 1920,
                                       default_video_height = 1080) {
  
  # 1. Find all CSV files in the folder
  cat(sprintf("Scanning for CSV files in '%s'...\n", folder_path))
  csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  if (length(csv_files) == 0) {
    cat("No CSV files found in the specified folder.\n")
    return(invisible(NULL))
  }
  cat(sprintf("Found %d CSV files.\n", length(csv_files)))
  
  # 2. Prepare inputs for the batch conversion function
  csv_inputs <- list()
  video_metadata_list <- list()
  
  for (i in seq_along(csv_files)) {
    csv_file_path <- csv_files[i]
    # Extract a clean name for the video from the filename
    video_name <- tools::file_path_sans_ext(basename(csv_file_path))
    
    # Create the list entries
    csv_inputs[[video_name]] <- csv_file_path
    video_metadata_list[[video_name]] <- list(
      id = i,
      name = video_name,
      width = default_video_width,
      height = default_video_height
    )
  }
  
  # 3. Define the output path within the source folder
  output_path <- file.path(folder_path, output_filename)
  
  # 4. Call the existing batch conversion function with the generated inputs
  convert_batch_to_kwcoco_r(
    csv_inputs = csv_inputs,
    video_metadata_list = video_metadata_list,
    output_path = output_path,
    col_mapping = col_mapping,
    default_category_id = default_category_id
  )
}

#' Converts all CSV files in a GCS folder to a single KWCOCO JSON file.
#'
#' This function authenticates with GCS, scans a bucket/prefix for .csv files,
#' and then calls the batch converter to produce a single KWCOCO file.
#'
#' @param bucket_name The name of the Google Cloud Storage bucket.
#' @param folder_path The prefix/folder path within the bucket.
#' @param output_path The local path to save the final JSON file.
#' @param col_mapping A named list that maps standard KWCOCO concepts to CSV column names.
#' @param ... Additional arguments passed to gcs_auth().
#'
convert_gcs_folder_to_kwcoco_r <- function(bucket_name,
                                           folder_path = "",
                                           output_path = "cloud_output_kwcoco.json",
                                           col_mapping,
                                           ...) {
  
  # 1. Authenticate with Google Cloud Storage
  cat("Authenticating with Google Cloud Storage...\n")
  gcs_auth(...)
  
  # 2. Find all CSV files in the GCS folder
  cat(sprintf("Scanning for CSV files in bucket '%s' with prefix '%s'...\n", bucket_name, folder_path))
  objects <- gcs_list_objects(bucket = bucket_name, prefix = folder_path)
  csv_object_names <- objects$name[grepl("\\.csv$", objects$name)]
  
  if (length(csv_object_names) == 0) {
    cat("No CSV files found in the specified GCS folder.\n")
    return(invisible(NULL))
  }
  cat(sprintf("Found %d CSV files.\n", length(csv_object_names)))
  
  # 3. Prepare inputs for the batch conversion function
  # The paths must be in the gs://bucket/object format for our pre-processor
  csv_inputs <- setNames(
    paste0("gs://", bucket_name, "/", csv_object_names),
    tools::file_path_sans_ext(basename(csv_object_names))
  )
  
  # Create placeholder video metadata
  video_metadata_list <- lapply(seq_along(csv_inputs), function(i) {
    list(id = i, name = names(csv_inputs)[i], width = 1920, height = 1080)
  })
  names(video_metadata_list) <- names(csv_inputs)
  
  # 4. Call the existing batch conversion function
  # The preprocess_and_read_csv function will handle the gs:// paths
  convert_batch_to_kwcoco_r(csv_inputs, video_metadata_list, output_path, col_mapping)
}

#' Converts all CSV files in a folder to individual KWCOCO JSON files. 
#' 
#' This function scans a directory for .csv files, automatically generates 
#' video metadata, and then calls the single file converter to produce an
#' individual KWCOCO file for each CSV, saved in the specified output folder.
#'
#' @param input_folder Path to the directory containing the input CSV files.
#' @param output_folder Path to the directory where the output JSON files will be saved.
#' @param col_mapping A named list that maps standard KWCOCO concepts to CSV column names.
#' @param default_category_id A default category ID to use if no category mapping is provided.
#' @param default_video_width Default width to use for video metadata.
#' @param default_video_height Default height to use for video metadata.
#'
convert_folder_to_individual_kwcoco_r <- function(input_folder,
                                                  output_folder,
                                                  col_mapping,
                                                  default_category_id = 1,
                                                  default_video_width = 1920,
                                                  default_video_height = 1080) {
  
  # Ensure output folder exists
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }
  
  # 1. Find all CSV files in the input folder
  cat(sprintf("Scanning for CSV files in '%s'...\n", input_folder))
  csv_files <- list.files(path = input_folder, pattern = "\\.csv$", full.names = TRUE)
  
  if (length(csv_files) == 0) {
    cat("No CSV files found in the specified input folder.\n")
    return(invisible(NULL))
  }
  cat(sprintf("Found %d CSV files. Beginning individual conversions...\n", length(csv_files)))
  
  # 2. Loop through each CSV and convert it
  for (i in seq_along(csv_files)) {
    csv_file_path <- csv_files[i]
    video_name <- tools::file_path_sans_ext(basename(csv_file_path))
    output_path <- file.path(output_folder, paste0(video_name, ".coco.json"))
    
    video_info <- list(
      id = i,
      name = video_name,
      width = default_video_width,
      height = default_video_height
    )
    
    convert_track_csv_to_kwcoco_r(
      csv_path = csv_file_path,
      video_info = video_info,
      output_path = output_path,
      col_mapping = col_mapping,
      default_category_id = default_category_id
    )
  }
  
  cat(sprintf("\nAll %d individual conversions completed. Outputs saved to '%s'.\n", length(csv_files), output_folder))
}


# --- HOW TO USE FOR YOUR OWN FILES ---

# The column_map tells the script how to find the data in your CSVs.
# You MUST configure this to match your column names.
your_column_map <- list(
  frame = "UniqFrame",
  track_id = "TrackID",
  tl_x = "TL_X",
  tl_y = "TL_Y",
  br_x = "BR_X",
  br_y = "BR_Y",
  score = "DetLen_Conf",
  # The 'category_id' will be the species name itself.
  species_name = "SP"
)

# --- Option 1: Convert a LOCAL folder ---
# To run the conversion on your local folder, you would uncomment and run the following lines:
#
# convert_folder_to_kwcoco_r(
#   folder_path = "/path/to/your/csv/folder",
#   output_filename = "my_kwcoco_dataset.json",
#   col_mapping = your_column_map
# )

# --- Option 2: Convert a Google Cloud Storage (GCS) folder ---
# To run the conversion on a GCS folder, you would uncomment and run the following lines:
#
# convert_gcs_folder_to_kwcoco_r(
#   bucket_name = "your-gcs-bucket-name",
#   folder_path = "path/inside/bucket/",
#   output_path = "my_cloud_kwcoco_dataset.json", # Local path to save the final file
#   col_mapping = your_column_map
# )

# --- Option 3: Convert a SINGLE local file ---
# To run the conversion on a single CSV file, you would uncomment and run the following lines:
#
# convert_track_csv_to_kwcoco_r(
#   csv_path = "C:/Users/Kelsey.l.martin.NMFS/Documents/NMFS/Automation/VIAME Output Analysis/Data/Tracks/2024_2.5/2024-SFD-022_tracks.csv",
#   video_info = list(id = 1, name = "/2024-SFD-022_tracks", width = 1920, height = 1080),
#   output_path = "C:/Users/Kelsey.l.martin.NMFS/Documents/NMFS/Automation/VIAME Output Analysis/Data/Tracks/2024_2.5/2024-SFD-022_tracks.coco.json",
#   col_mapping = your_column_map
# )

# --- Option 4: Convert a LOCAL folder to INDIVIDUAL files ---
# To run the conversion on a local folder and create separate JSON files for each CSV,
# you would uncomment and run the following lines:
#
convert_folder_to_individual_kwcoco_r(
  input_folder = "G:/.shortcut-targets-by-id/1NQbXTERaMRHyQASygwppJ54a6DoC2n0H/AI ML Materials/Reef Fish Training Library/training library/For_Training",
  output_folder = "G:/.shortcut-targets-by-id/1NQbXTERaMRHyQASygwppJ54a6DoC2n0H/AI ML Materials/Reef Fish Training Library/training library/For_Training_KWCOCO_Files",
  col_mapping = your_column_map
)

convert_track_csv_to_kwcoco_r(
  csv_path = "C:/Users/Kelsey.l.martin.NMFS/Downloads/AUV_viame_test_detections.csv",
  video_info = list(id = 1, name = "/AUV_viame_test_detections", width = 1920, height = 1080),
  output_path = "C:/Users/Kelsey.l.martin.NMFS/Downloads/AUV_viame_test_detections.coco.json",
  col_mapping = your_column_map
)



# --- SELF-CONTAINED TEST & DEMONSTRATION ---
# The code below is a self-contained test. It creates a temporary folder and
# sample CSV files to demonstrate that the script is working correctly.
# You can run this part of the script as-is to verify its functionality.

run_demonstration <- function() {
  cat("\n--- Running Self-Contained Demonstration ---\n")
  temp_folder <- "temp_csv_folder"
  if (!dir.exists(temp_folder)) { dir.create(temp_folder) }
  
  df1 <- data.frame(TrackID=c(1,2,1), VidIdent="v1", UniqFrame=c(1,1,2), TL_X=c(10,100,12), TL_Y=c(10,150,12), BR_X=c(30,150,32), BR_Y=c(50,230,52), DetLen_Conf=c(0.98,0.95,0.99), Tar_Len=10, SP=1, CP=2, Deployment="2022-Dep1")
  df2 <- data.frame(TrackID=c(3,3,4), VidIdent="v2", UniqFrame=c(1,2,2), TL_X=c(50,55,200), TL_Y=c(50,55,250), BR_X=c(75,80,255), BR_Y=c(95,100,335), DetLen_Conf=c(0.91,0.92,0.89), Tar_Len=12, SP=1, CP=3, Deployment="Dep2")
  # Write the dummy dataframes without headers and with 2 junk rows to simulate the real scenario
  write(c("Junk Header Row 1", "Junk Header Row 2"), file.path(temp_folder, "2022_video1_tracks.csv"))
  write_csv(df1, file.path(temp_folder, "2022_video1_tracks.csv"), append = TRUE, col_names = FALSE)
  write(c("Junk Header Row 1", "Junk Header Row 2"), file.path(temp_folder, "video2_tracks.csv"))
  write_csv(df2, file.path(temp_folder, "video2_tracks.csv"), append = TRUE, col_names = FALSE)
  cat(sprintf("Created dummy CSV files in folder: '%s'\n", temp_folder))
  
  convert_folder_to_kwcoco_r(
    folder_path = temp_folder,
    output_filename = "folder_combined_output.json",
    col_mapping = your_column_map
  )
  cat(sprintf("\nSuccessfully converted all CSVs in '%s' to '%s'.\n", temp_folder, file.path(temp_folder, "folder_combined_output.json")))
  cat("--- Demonstration Complete ---\n")
}

# To run the demonstration, uncomment the line below:
# run_demonstration()
