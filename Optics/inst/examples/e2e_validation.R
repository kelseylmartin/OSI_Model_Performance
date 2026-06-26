# End-to-End Data Processing Example for the Optics Package
#
# This script demonstrates how to use the core functions of the Optics package
# to go from raw model outputs to performance metrics using the user's
# specified data files.

# --- 1. Setup: Load Package and File Paths ---

# In a development context, it's more reliable to source files directly
# than to rely on a potentially cached installed version of the package.
r_files <- list.files("Optics/R", pattern = "\\.R$", full.names = TRUE)
for (file in r_files) {
  source(file)
}
library(dplyr)

# Define file paths for model and truth data.
model_csv_path <- "Optics/inst/extdata/2024-NCD-017_tracks_SUBSET.csv"
truth_csv_path <- "Optics/inst/extdata/maxn3LABS_93to24.csv"

# --- 2. Ingest and Process Data ---

# Ingest the model detections from the VIAME CSV.
cat("Ingesting model detections from:", model_csv_path, "\n")
model_detections <- read_viame_csv(model_csv_path)
cat("Model detections ingested successfully.\n\n")

# Process the raw model detections to get MaxN counts per species.
cat("Calculating MaxN for model detections...\n")
model_maxn <- calculate_maxn(model_detections)
print(head(model_maxn))
cat("\n")

# Ingest and process the wide-format truth data.
# The `read_wide_maxn` function reads the CSV, pivots it to a long format,
# and renames columns to be compatible with the alignment functions.
cat("Ingesting and transforming wide-format truth data from:", truth_csv_path, "\n")
truth_maxn <- read_wide_maxn(truth_csv_path, video_id_col = REFERENCE)
cat("Transformed truth data (MaxN counts):\n")
print(head(truth_maxn))
cat("\n")


# --- 3. Align Counts ---

cat("Aligning model and truth counts...\n")
# The 'by' columns are critical for matching rows from model and truth.
# Note: The model `video_id` is a filename, and the truth `video_id` is a
# reference number. These will not match up. The alignment will still work,
# but all data will be treated as unmatched (FPs from model, FNs from truth).
# This demonstrates the functionality, but a real analysis would require
# consistent video identifiers.
aligned_df <- align_counts(model_maxn, truth_maxn, by = c("video_id", "category_name"), truth_col = truth_count)
cat("Aligned counts (sample):\n")
print(head(aligned_df))
cat("\n")


# --- 4. Calculate Performance Metrics ---

cat("Calculating binary performance metrics...\n")
binary_metrics <- calculate_binary_metrics(aligned_df)
print(binary_metrics)
cat("\n")

cat("Calculating confusion matrix...\n")
confusion_matrix <- calculate_confusion_matrix(aligned_df, group_vars = c("video_id", "category_name"), species_col = category_name)
print(confusion_matrix)
cat("\n")

cat("Generating disagreement report...\n")
disagreement_report <- get_disagreement_report(aligned_df, group_vars = c("video_id", "category_name"))
print(head(disagreement_report))
cat("\n")


# --- 5. Inapplicable Functions ---

cat("--- Notes on other functions ---\n")
cat("The following functions cannot be run with the pre-aggregated truth data format.\n\n")

cat("`summarize_performance_by_threshold`: This function requires a raw, un-aggregated\n")
cat("`OpticsDetections` object for the truth data so it can re-calculate metrics at\n")
cat("different model confidence thresholds. The `read_wide_maxn` output is already aggregated.\n\n")

cat("`classify_detections`: Requires a unique detection ID present in both model and truth\n")
cat("to match individual detections and classify them as TP/FP.\n\n")

cat("`analyze_reviewer_effort`: Requires a 'TrackID' and 'Species' column for comparing\n")
cat("a raw dataframe to a validated one to see how classifications changed.\n\n")

cat("`analyze_performance_drivers`: A complex function that fits a statistical model.\n\n")

cat("End-to-end script finished.\n")

