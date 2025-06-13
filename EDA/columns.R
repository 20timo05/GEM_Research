# Load necessary libraries
library(data.table) # Only data.table is strictly needed for fread

# --- Define Project Structure & Load Data ---

# Determine the base project directory.
# This assumes the script is placed in 'EDA' folder within your 'Machine_Learning' project.
# Example: C:/Users/timor/Desktop/Hochschule/Machine_Learning/EDA/your_script.R
base_project_dir <- file.path(dirname(rstudioapi::getActiveDocumentContext()$path), "..")
if (is.null(base_project_dir) || base_project_dir == "..") { # Fallback if not in RStudio or script not saved
  base_project_dir <- file.path(getwd(), "..") # Assumes you run it from 'EDA' folder
}

# Define and create output path
output_path <- file.path(base_project_dir, "EDA", "output")
dir.create(output_path, recursive = TRUE, showWarnings = FALSE) # Creates EDA/output if they don't exist

# Define data path and load data
data_path <- file.path(base_project_dir, "data", "GEM2021APSGlobalIndividualLevelData_15Feb2023.csv")
cat(sprintf("Loading data from: %s\n", data_path))
gem_data <- fread(data_path)
cat("Data loaded successfully.\n")

# --- Function to print unique values to file ---
print_unique_values_to_file <- function(data_frame, file_path) {
  con <- file(file_path, "w")
  on.exit(close(con))

  if (is.null(data_frame) || nrow(data_frame) == 0) {
    writeLines("The data frame is empty or NULL.\n", con = con)
    return()
  }

  writeLines("--- Unique Values Per Column ---\n", con = con)
  for (col_name in names(data_frame)) {
    writeLines(sprintf("\nColumn: %s\n", col_name), con = con)
    unique_vals <- unique(data_frame[[col_name]])
    
    if (length(unique_vals) > 0) {
      if (length(unique_vals) <= 20) {
        writeLines(paste0("  Unique values: ", paste(unique_vals, collapse = ", "), "\n"), con = con)
      } else {
        writeLines(sprintf("  Number of unique values: %d\n", length(unique_vals)), con = con)
        writeLines(paste0("  First 20 unique values: ", paste(head(unique_vals, 20), collapse = ", "), "...\n"), con = con)
      }
    } else {
      writeLines("  No unique values found (column might be entirely NA).\n", con = con)
    }
  }
  writeLines("\n--- End of Unique Values Report ---\n", con = con)
  cat(sprintf("Unique values report successfully written to: %s\n", file_path))
}

# --- Execute ---
print_unique_values_to_file(gem_data, file.path(output_path, "unique_values_report.txt"))