---
title: "Introduction to the Optics Package"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Introduction to the Optics Package}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



## 1. Introduction

The `Optics` package provides a standardized toolkit for evaluating the performance of machine learning models on optical survey data. This vignette demonstrates a current workflow using bundled example data, from ingesting raw model output to generating final performance metrics and visualizations.

<div class="vehicle-icons"><img src="https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTuDMP7FiViQpYFSlWHisRaeY-JnzHHM4fl6EBkjbwCkw&s=10" alt="NOAA Research Vessel" class="platform-icon boat"/><img src="https://s1.cdn.autoevolution.com/images/news/gallery/modified-cessna-caravan-kicks-off-nasa-s-research-into-remotely-piloted-cargo-planes_2.jpg" alt="Small Propeller Plane" class="platform-icon plane"/><img src="https://res.cloudinary.com/osrl-production/image/upload/osrlprod/globalassets/knowledge-hub-169/smv/auv-slocum-glider.png" alt="Autonomous Underwater Vehicle" class="platform-icon auv"/></div>

First, let's load the `Optics` package and other useful libraries like `dplyr`.


```r
library(Optics)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(ggplot2)
```

## 2. Loading Packaged Example Data

For this example, we will use the bundled AKFSC ice seal files so the homepage reflects the current package workflows. This gives us a realistic walkthrough of data ingestion, candidate selection, threshold summaries, and visualization using the same helper functions that support the full package vignettes and reports.


```r
extdata_dir <- system.file("extdata/AKFSC", package = "Optics")
if (extdata_dir == "") {
  extdata_dir <- file.path("inst", "extdata", "AKFSC")
}

model_files <- list.files(extdata_dir, pattern = "_ir_detections\\.csv$", full.names = TRUE)
truth_files <- list.files(extdata_dir, pattern = "_validated\\.csv$", full.names = TRUE)

camera_from_path <- function(path) {
  stringr::str_match(basename(path), "_([LCR])_")[, 2]
}

camera_file_map <- dplyr::inner_join(
  tibble::tibble(model_file = model_files, camera = camera_from_path(model_files)),
  tibble::tibble(truth_file = truth_files, camera = camera_from_path(truth_files)),
  by = "camera"
) %>%
  arrange(camera)

model_detections <- ingest_ice_seals_csv(camera_file_map$model_file)
truth_detections <- ingest_ice_seals_csv(camera_file_map$truth_file)

truth_file_map <- setNames(camera_file_map$truth_file, camera_file_map$camera)
truth_totals <- calculate_ice_seals_totals(
  left_file = truth_file_map[["L"]],
  center_file = truth_file_map[["C"]],
  right_file = truth_file_map[["R"]]
)

candidate_detections <- select_ice_seals_candidates(
  model_detections,
  score_threshold = 0.7,
  candidate_class = "hotspot"
)

print(truth_totals)
#> $left
#> [1] 121
#> 
#> $center
#> [1] 78
#> 
#> $right
#> [1] 105

candidate_detections %>%
  select(image_id, frame_index, category_name, score) %>%
  head()
#>                                                                                                                                                     image_id
#> 1 \\\\akc0ss-n086\\NMML_Polar_Imagery_4\\Surveys_IceSeals_2025\\fl223\\images_30deg_N68RF\\center_view\\ice_seals_2025_fl223_C_20250513_223137.115060_ir.tif
#> 2 \\\\akc0ss-n086\\NMML_Polar_Imagery_4\\Surveys_IceSeals_2025\\fl223\\images_30deg_N68RF\\center_view\\ice_seals_2025_fl223_C_20250513_223646.824115_ir.tif
#> 3 \\\\akc0ss-n086\\NMML_Polar_Imagery_4\\Surveys_IceSeals_2025\\fl223\\images_30deg_N68RF\\center_view\\ice_seals_2025_fl223_C_20250513_225201.625272_ir.tif
#> 4 \\\\akc0ss-n086\\NMML_Polar_Imagery_4\\Surveys_IceSeals_2025\\fl223\\images_30deg_N68RF\\center_view\\ice_seals_2025_fl223_C_20250513_225746.545744_ir.tif
#> 5 \\\\akc0ss-n086\\NMML_Polar_Imagery_4\\Surveys_IceSeals_2025\\fl223\\images_30deg_N68RF\\center_view\\ice_seals_2025_fl223_C_20250513_225747.614950_ir.tif
#> 6 \\\\akc0ss-n086\\NMML_Polar_Imagery_4\\Surveys_IceSeals_2025\\fl223\\images_30deg_N68RF\\center_view\\ice_seals_2025_fl223_C_20250513_231236.053040_ir.tif
#>   frame_index category_name   score
#> 1         130       hotspot 0.71061
#> 2         398       hotspot 0.70879
#> 3        1145       hotspot 0.71065
#> 4        1462       hotspot 0.77292
#> 5        1463       hotspot 0.72416
#> 6        2253       hotspot 0.73261
```

## 3. Optional: Build a GCP Media URI Index

When your workflow inputs are stored in Google Cloud Storage, `scrape_gcp_uris()` can build a simple media index for downstream ingestion. This keeps bucket listing logic centralized and returns a consistent three-column lookup table (`folder_name`, `file_name`, `bucket_uri`).

```r
gcp_media_index <- scrape_gcp_uris(
  bucket_name = "nmfs-dev-uc1-sefsc",
  prefix = "GFISHER/Video_data/",
  extensions = c(".mp4", ".avi", ".jpg")
)

head(gcp_media_index)
```

## 4. Summarizing Performance by Threshold

A key task is to evaluate how a model performs at different confidence thresholds. For a compact homepage example, we'll create small in-memory detection tables, convert them to `OpticsDetections` objects, and summarize performance across thresholds with `summarize_performance_by_threshold()`.


```r
# Sample model detections with scores
model_detections_df <- tibble(
  video_id = "vid01",
  image_id = "img01",
  annotation_id = 1:8,
  category_name = c("Gadus morhua", "Gadus morhua", "Melanogrammus aeglefinus", "Gadus morhua", "Pollachius virens", "Gadus morhua", "Melanogrammus aeglefinus", "Melanogrammus aeglefinus"),
  score = c(0.95, 0.85, 0.80, 0.65, 0.50, 0.92, 0.75, 0.60),
  frame_index = c(10, 10, 15, 20, 20, 10, 15, 15),
  model_name = c(rep("Model A", 5), rep("Model B", 3)),
  bbox_x = 0, bbox_y = 0, bbox_width = 1, bbox_height = 1
)

# Sample ground truth detections
truth_detections_df <- tibble(
  video_id = "vid01",
  image_id = "img01",
  annotation_id = 9:12,
  category_name = c("Gadus morhua", "Gadus morhua", "Gadus morhua", "Urophycis tenuis"),
  frame_index = c(10, 10, 20, 30),
  score = 1.0,
  bbox_x = 0, bbox_y = 0, bbox_width = 1, bbox_height = 1
)

thresholds_to_test <- seq(0.5, 1.0, by = 0.1)

model_a <- OpticsDetections(
  data = filter(model_detections_df, model_name == "Model A"),
  source_file = "manual",
  ingest_format = "manual"
)
model_b <- OpticsDetections(
  data = filter(model_detections_df, model_name == "Model B"),
  source_file = "manual",
  ingest_format = "manual"
)
truth <- OpticsDetections(
  data = truth_detections_df,
  source_file = "manual",
  ingest_format = "manual"
)

perf_model_a <- summarize_performance_by_threshold(
  model_detections = model_a,
  truth_detections = truth,
  by = c("video_id", "category_name"),
  thresholds = thresholds_to_test
) %>% mutate(model_name = "Model A")

perf_model_b <- summarize_performance_by_threshold(
  model_detections = model_b,
  truth_detections = truth,
  by = c("video_id", "category_name"),
  thresholds = thresholds_to_test
) %>% mutate(model_name = "Model B")

performance_summary <- bind_rows(perf_model_a, perf_model_b)

performance_summary %>%
  select(model_name, threshold, precision, recall, f1_score) %>%
  arrange(model_name, threshold)
#> # A tibble: 12 × 5
#>    model_name threshold precision recall f1_score
#>    <chr>          <dbl>     <dbl>  <dbl>    <dbl>
#>  1 Model A          0.5     0.333    0.5    0.4  
#>  2 Model A          0.6     0.5      0.5    0.5  
#>  3 Model A          0.7     0.5      0.5    0.5  
#>  4 Model A          0.8     0.5      0.5    0.5  
#>  5 Model A          0.9     1        0.5    0.667
#>  6 Model A          1      NA        0     NA    
#>  7 Model B          0.5     0.5      0.5    0.5  
#>  8 Model B          0.6     0.5      0.5    0.5  
#>  9 Model B          0.7     0.5      0.5    0.5  
#> 10 Model B          0.8     1        0.5    0.667
#> 11 Model B          0.9     1        0.5    0.667
#> 12 Model B          1      NA        0     NA
```

## 5. Visualizing Performance

With the summary data, we can now create plots to compare the model views. The plotting functions are now S4 generics and work directly with the current threshold-summary output.

### Performance by Threshold Plot

The `plot_performance_by_threshold()` function visualizes the trade-offs between precision, recall, and F1-score.


```r
plot_performance_by_threshold(
  performance_summary,
  model_col = model_name,
  title = "Model Performance Comparison"
)
#> Warning: Removed 2 rows containing missing values (`geom_line()`).
#> Warning: Removed 4 rows containing missing values (`geom_point()`).
```

![Precision, Recall, and F1-score for Model A and Model B across different confidence thresholds.](figure/plot-performance-1.png)

### Count Comparison Scatterplot

To see how well the model counts match the truth counts at a specific threshold, we can generate a scatterplot. The `calculate_maxn()` generic works on both `OpticsDetections` objects and standard `data.frame`s.


```r
model_counts_at_08 <- model_detections_df %>%
  filter(score >= 0.8) %>%
  calculate_maxn(group_cols = "model_name")

truth_counts <- calculate_maxn(truth_detections_df)

aligned_a <- align_counts(
  model_counts = filter(model_counts_at_08, model_name == "Model A"),
  truth_counts = truth_counts,
  by = c("video_id", "category_name")
) %>% mutate(model_name = "Model A")

aligned_b <- align_counts(
  model_counts = filter(model_counts_at_08, model_name == "Model B"),
  truth_counts = truth_counts,
  by = c("video_id", "category_name")
) %>% mutate(model_name = "Model B")

aligned_counts <- bind_rows(aligned_a, aligned_b)

plot_counts_scatterplot(
  aligned_counts,
  model_col = model_name,
  title = "MaxN Counts at 0.8 Confidence"
)
```

![Model vs. truth MaxN counts at a 0.8 confidence threshold.](figure/plot-scatterplot-1.png)

## 6. Strict S4 Class Reports

Use `OpticsDetections` objects from `read_viame_csv()` or `read_kwcoco()` for the strict S4 workflow. Alignment returns an `OpticsPerformance` object with an initially empty `@metrics` slot.

```r
model <- read_viame_csv("model_tracks.csv", video_id = "survey01")
truth <- read_kwcoco("truth.kwcoco.json")
performance <- align_counts(
  model, truth,
  grouping_level = "frame",
  confidence_thresholds = c(0.5, 0.9)
)
performance <- calculate_precision(performance)
performance <- calculate_f1(performance)
generate_class_report(
  performance, class_label = "Gadus morhua",
  output_dir = "reports",
  remove_large_schools = FALSE
)
```

Model and truth must use matching video IDs and category names. Choose `grouping_level = "frame"` for frame abundance or `"video"` for MaxN when aligning. You cannot regroup after alignment: realign the original detections to change grouping level.

Each individual `calculate_*()` function adds only its named lower-case metric column to `performance@metrics`, aggregated by `@group_vars`; intermediate dependencies are not added. Optional `confidence_thresholds` select already aligned thresholds, not new detection-score cutoffs. `generate_class_report()` reports only chosen metrics (here, `precision` and `f1`).

The report's `remove_large_schools` toggle defaults to `FALSE`. Opting in with `TRUE` excludes whole aligned observations with truth codes 299, 399, or 999 and recalculates only the chosen metrics on a copy. It does not recode truth or cap model counts.

### Generate and open the analysis report

Install Optics first, then install the report's suggested packages with `install.packages(c("rmarkdown", "knitr"))`. Pandoc must be available (`rmarkdown::pandoc_available()` should return `TRUE`); RStudio normally bundles it. No cloud credentials are needed for the bundled example.

`class_label` must exactly match a category in `performance@data`. Calculate the desired metrics before calling `generate_class_report()`: only those metrics receive tables and plots. Set `output_file = "my-class-report.html"` to choose the filename; `output_dir` is created if needed. The function invisibly returns the HTML path, which you can open in a browser:

```r
report_path <- generate_class_report(
  performance, class_label = "Gadus morhua",
  output_file = "cod-performance.html", output_dir = "reports"
)
utils::browseURL(normalizePath(report_path))
```

### Complete GFISHER example

The installed [`gfisher_class_report.R`](inst/examples/gfisher_class_report.R) script runs every prerequisite using bundled GFISHER data: ingest the original `2024-NCO-155_tracks.csv`, match species names, restrict frames to the manual reading window, calculate model MaxN at each threshold, align against manual deployment MaxN, calculate all 17 modular metrics on an `OpticsPerformance` object, and generate the class report.

```r
source(system.file("examples", "gfisher_class_report.R", package = "Optics"))
result <- run_gfisher_class_report(
  class_label = "LUTJANUS_CAMPECHANUS",
  confidence_thresholds = c(0.5, 0.9),
  output_dir = "reports", remove_large_schools = FALSE
)
result$performance@metrics
utils::browseURL(normalizePath(result$report_path))
```

Alternatively, run `Rscript /path/to/gfisher_class_report.R reports`. The default output is `reports/lutjanus_campechanus-performance.html`. You can also select `BALISTES_CAPRISCUS` or `RHOMBOPLITES_AURORUBENS`. To report fewer metrics, omit their individual calculator calls in the script.

This is a small walkthrough, not a survey-wide performance estimate: it uses one deployment and three unambiguously matched species, not duplicate `_ST`/`_SUBSET` track variants. Manual truth is already MaxN, so it is aligned as a count table and then wrapped in S4 rather than fabricated into frame detections. Recorded zero counts are retained; undefined rates (such as FPR when no actual negatives exist) remain `NA`. For other deployments, update the file, identifier, reading window, and species crosswalk together.

For existing count tables, construct `OpticsPerformance(data, grouping_level = "video", group_vars = c("category_name", "threshold"))` directly. `data` must contain `category_name`, `threshold`, `model_count`, and `truth_count`; extra grouping columns such as `year` and `Version` can be included in `group_vars` if present in `data`. Construction starts with no calculated metrics. The GFISHER rewrite scripts use this approach and rename selected lower-case metrics to the existing report table names before calling `calculate_percent_metric()`.

This vignette provides a basic overview of the current package workflow. The `Optics` package also includes helpers for Ice Seal review preparation (`ingest_ice_seals_csv()`, `calculate_ice_seals_totals()`, `select_ice_seals_candidates()`), GFISHER percentage summaries (`calculate_percent_metric()`), and more in-depth analyses such as ROC curves, confusion matrices, and reviewer-effort summaries.
