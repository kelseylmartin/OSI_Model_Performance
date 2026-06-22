
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)
library(Optics)
library(dplyr)
library(ggplot2)
model_detections_df <- tibble(
  video_id = "vid01",
  image_id = "img01",
  annotation_id = 1:8,
  category_name = c("Gadus morhua", "Gadus morhua", "Melanogrammus aeglefinus", "Gadus morhua", "Pollachius virens", "Gadus morhua", "Melanogrammus aeglefinus", "Melanogrammus aeglefinus"),
  score = c(0.95, 0.85, 0.80, 0.65, 0.50, 0.92, 0.75, 0.60),
  frame_index = c(10, 10, 15, 20, 20, 10, 15, 15),
  model_name = c(rep("Model A", 5), rep("Model B", 3)),
  bbox_x = 0, bbox_y = 0, bbox_width = 1, bbox_height = 1 # Dummy columns for validation
)
truth_detections_df <- tibble(
  video_id = "vid01",
  image_id = "img01",
  annotation_id = 9:12,
  category_name = c("Gadus morhua", "Gadus morhua", "Gadus morhua", "Urophycis tenuis"),
  frame_index = c(10, 10, 20, 30),
  score = 1.0, # Dummy score for validation
  bbox_x = 0, bbox_y = 0, bbox_width = 1, bbox_height = 1 # Dummy columns for validation
)

thresholds_to_test <- seq(0.5, 1.0, by = 0.1)
model_a_obj <- OpticsDetections(
  data = filter(model_detections_df, model_name == "Model A"),
  source_file = "manual", ingest_format = "manual"
)
model_b_obj <- OpticsDetections(
  data = filter(model_detections_df, model_name == "Model B"),
  source_file = "manual", ingest_format = "manual"
)
truth_obj <- OpticsDetections(
  data = truth_detections_df,
  source_file = "manual", ingest_format = "manual"
)
perf_model_a <- summarize_performance_by_threshold(
  model_detections = model_a_obj,
  truth_detections = truth_obj,
  by = c("video_id", "category_name"),
  thresholds = thresholds_to_test
) %>% mutate(model_name = "Model A")
perf_model_b <- summarize_performance_by_threshold(
  model_detections = model_b_obj,
  truth_detections = truth_obj,
  by = c("video_id", "category_name"),
  thresholds = thresholds_to_test
) %>% mutate(model_name = "Model B")
performance_summary <- bind_rows(perf_model_a, perf_model_b)
print(performance_summary)
