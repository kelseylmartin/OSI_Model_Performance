# ---
# End-to-End Example: Ice Seals Dataset
# ---

library(Optics)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)

# --- 1. Discover all model and validated files from extdata ---
extdata_dir <- system.file("extdata/AKFSC", package = "Optics")

model_files <- list.files(
  extdata_dir,
  pattern = "_detections\\.csv$",
  full.names = TRUE
)
truth_files <- list.files(
  extdata_dir,
  pattern = "_validated\\.csv$",
  full.names = TRUE
)

extract_camera <- function(path) {
  camera <- str_match(basename(path), "_([LCR])_")[, 2]
  ifelse(is.na(camera), "", camera)
}

model_index <- tibble(model_file = model_files, camera = extract_camera(model_files))
truth_index <- tibble(truth_file = truth_files, camera = extract_camera(truth_files))

if (any(model_index$camera == "") || any(truth_index$camera == "")) {
  stop("Could not extract camera position (L/R/C) from one or more file names.")
}
if (anyDuplicated(model_index$camera) || anyDuplicated(truth_index$camera)) {
  stop("Expected one model and one validated file per camera (L/R/C).")
}
if (!setequal(model_index$camera, truth_index$camera)) {
  stop("Model and validated camera sets do not match.")
}

camera_file_map <- inner_join(model_index, truth_index, by = "camera")

# --- 2. Ingest all matched camera files into S4 objects ---
model_detections <- ingest_ice_seals_csv(camera_file_map$model_file)
truth_detections <- ingest_ice_seals_csv(camera_file_map$truth_file)

# --- 3. Subset model detections to images that have validated GT ---
extract_camera_from_image <- function(image_id) str_match(basename(image_id), "_([LCR])_")[, 2]

model_df <- model_detections@data %>%
  mutate(camera = extract_camera_from_image(image_id))
truth_df <- truth_detections@data %>%
  mutate(camera = extract_camera_from_image(image_id))

truth_image_index <- truth_df %>% distinct(camera, image_id)

model_matched_df <- model_df %>%
  semi_join(truth_image_index, by = c("camera", "image_id"))

truth_matched_df <- truth_df %>%
  semi_join(distinct(model_matched_df, camera, image_id), by = c("camera", "image_id"))

model_matched <- OpticsDetections(
  data = select(model_matched_df, -camera),
  source_file = paste(camera_file_map$model_file, collapse = ", "),
  ingest_format = "viame_csv_ice_seals_matched"
)
truth_matched <- OpticsDetections(
  data = select(truth_matched_df, -camera),
  source_file = paste(camera_file_map$truth_file, collapse = ", "),
  ingest_format = "viame_csv_ice_seals_matched"
)

# --- 4. Frame-abundance alignment (image subset only) ---
model_frame_abundance <- calculate_frame_abundance(model_matched)
truth_frame_abundance <- calculate_frame_abundance(truth_matched)

aligned_counts <- align_counts(
  model_frame_abundance,
  truth_frame_abundance,
  by = c("video_id", "frame_index", "category_name"),
  model_col = abundance,
  truth_col = abundance
) %>%
  rename(class_label = category_name, true_count = truth_count)

cat("Aligned frame-abundance rows:", nrow(aligned_counts), "\n")

# --- 5. Many-to-one overlap analysis (extra detections per validated target) ---
bbox_iou <- function(ax, ay, aw, ah, bx, by, bw, bh) {
  ax2 <- ax + aw
  ay2 <- ay + ah
  bx2 <- bx + bw
  by2 <- by + bh

  inter_x1 <- pmax(ax, bx)
  inter_y1 <- pmax(ay, by)
  inter_x2 <- pmin(ax2, bx2)
  inter_y2 <- pmin(ay2, by2)

  inter_w <- pmax(0, inter_x2 - inter_x1)
  inter_h <- pmax(0, inter_y2 - inter_y1)
  inter_area <- inter_w * inter_h

  union_area <- (aw * ah) + (bw * bh) - inter_area
  ifelse(union_area <= 0, 0, inter_area / union_area)
}

model_overlap_df <- model_matched_df %>%
  select(camera, image_id, frame_index, annotation_id, category_name, bbox_x, bbox_y, bbox_width, bbox_height)
truth_overlap_df <- truth_matched_df %>%
  select(camera, image_id, frame_index, annotation_id, category_name, bbox_x, bbox_y, bbox_width, bbox_height)

overlap_counts <- inner_join(
  model_overlap_df,
  truth_overlap_df,
  by = c("camera", "image_id", "frame_index"),
  suffix = c("_model", "_truth")
) %>%
  mutate(
    iou = bbox_iou(
      bbox_x_model, bbox_y_model, bbox_width_model, bbox_height_model,
      bbox_x_truth, bbox_y_truth, bbox_width_truth, bbox_height_truth
    ),
    same_class = category_name_model == category_name_truth
  ) %>%
  filter(iou >= 0.5, same_class) %>%
  group_by(camera, image_id, frame_index, annotation_id_truth) %>%
  summarise(overlap_model_detections = n_distinct(annotation_id_model), .groups = "drop")

truth_targets <- truth_overlap_df %>%
  distinct(camera, image_id, frame_index, annotation_id_truth = annotation_id)

duplicate_summary <- truth_targets %>%
  left_join(overlap_counts, by = c("camera", "image_id", "frame_index", "annotation_id_truth")) %>%
  mutate(overlap_model_detections = replace_na(overlap_model_detections, 0L)) %>%
  mutate(extra_model_detections = pmax(overlap_model_detections - 1L, 0L)) %>%
  group_by(camera, image_id, frame_index) %>%
  summarise(
    truth_targets = n(),
    extra_model_detections = sum(extra_model_detections),
    .groups = "drop"
  )

cat("\nOverlap/duplicate summary (first rows):\n")
print(head(duplicate_summary))

# --- 6. Quantify reviewer correction of model classifications ---
correction_details <- model_matched_df %>%
  select(camera, image_id, frame_index, annotation_id, model_class_label = category_name) %>%
  inner_join(
    truth_matched_df %>%
      select(camera, image_id, frame_index, annotation_id, validated_class_label = category_name),
    by = c("camera", "image_id", "frame_index", "annotation_id")
  ) %>%
  mutate(class_changed = model_class_label != validated_class_label)

correction_summary <- correction_details %>%
  group_by(camera) %>%
  summarise(
    matched_detections = n(),
    changed_classifications = sum(class_changed),
    percent_changed = round(100 * changed_classifications / matched_detections, 2),
    .groups = "drop"
  )

cat("\nReviewer correction summary:\n")
print(correction_summary)

cat("\nIce seals end-to-end example complete.\n")
