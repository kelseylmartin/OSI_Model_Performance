# ---
# End-to-End Example: Ice Seals Dataset
# ---

# This script demonstrates the full workflow for processing the Ice Seals
# dataset, from data ingestion to performance analysis.

# --- 1. Load the Optics Package ---
library(Optics)
library(dplyr)

# --- 2. Define File Paths ---
# For this example, we use the raw detection and validated (ground truth)
# files included with the package.

# Raw model output files
model_files <- c(
  L = system.file("extdata", "ice_seals_2025_fl223_L_ir_detections.csv", package = "Optics"),
  C = system.file("extdata", "ice_seals_2025_fl223_C_ir_detections.csv", package = "Optics"),
  R = system.file("extdata", "ice_seals_2025_fl223_R_ir_detections.csv", package = "Optics")
)

# Ground truth files
truth_files <- c(
  L = system.file("extdata", "ice_seals_2025_fl223_L_ir_detections_validated.csv", package = "Optics"),
  C = system.file("extdata", "ice_seals_2025_fl223_C_ir_detections_validated.csv", package = "Optics"),
  R = system.file("extdata", "ice_seals_2025_fl223_R_ir_detections_validated.csv", package = "Optics")
)

# --- 3. Ingest Data ---
# Ingest the raw model detections and the ground truth data using the dedicated
# function for the Ice Seals dataset.
model_detections <- ingest_ice_seals_csv(model_files)
truth_detections <- ingest_ice_seals_csv(truth_files)

cat("--- Ingested Data ---
")
print(model_detections)
print(truth_detections)

# --- 4. Calculate and Validate Animal Totals (from Ground Truth) ---
# This step is specific to the Ice Seals project's quality control process.
cat("
--- Ground Truth Animal Totals ---
")
totals <- calculate_ice_seals_totals(
  left_file = truth_files["L"],
  center_file = truth_files["C"],
  right_file = truth_files["R"]
)
print(totals)


# --- 5. Select Candidate Detections for Review ---
# Identify high-confidence "animal" detections that may be sent for a
# secondary review (e.g., in RGB imagery).
cat("
--- Candidate Detections for Review ---
")
candidate_detections <- select_ice_seals_candidates(model_detections)
print(head(candidate_detections))


# --- 6. Align Model and Ground Truth Counts ---
# To evaluate performance, we first need to get counts (e.g., MaxN) from
# both the model and ground truth data, and then align them.

# Calculate MaxN per video and class
model_counts <- calculate_maxn(model_detections)
truth_counts <- calculate_maxn(truth_detections)

# Align the two count dataframes
aligned_counts <- align_counts(model_counts, truth_counts, by = c("video_id", "category_name"))

cat("
--- Aligned Model and Truth Counts (MaxN) ---
")
print(aligned_counts)


# --- 7. Analyze Performance ---
# With the aligned data, you can now use the various analysis and
# visualization functions in the Optics package.

# Example: Calculate binary classification metrics
# We are interested in the "animal" class.
binary_metrics <- aligned_counts %>%
  filter(category_name == "animal") %>%
  calculate_binary_metrics(model_count, truth_count)

cat("
--- Binary Classification Metrics for 'animal' Class ---
")
print(binary_metrics)

# Example: Plot a scatterplot of model vs. truth counts
p_scatter <- plot_counts_scatterplot(aligned_counts, model_col = model_count, truth_col = truth_count)

print(p_scatter)

cat("
--- Example script finished. ---
")
