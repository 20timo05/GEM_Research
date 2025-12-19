library(shiny)

required_files <- c(
  "output_production/final_model.rds",
  "output_production/final_threshold.rds",
  "output_production/shap_background.rds"
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  # Create a clean error message
  msg <- paste0(
    "\n\n====================================================================\n",
    "CRITICAL ERROR: Missing Production Artifacts\n",
    "====================================================================\n",
    "The following required files are missing from 'output_production/':\n",
    paste0(" - ", missing_files, collapse = "\n"),
    "\n\n",
    "ACTION REQUIRED:\n",
    "Please run the training script to generate these files before starting the GUI.\n",
    "Run: source('scripts_production/02_train_final_model.R')\n",
    "====================================================================\n"
  )
  
  # Stop execution so the app doesn't crash halfway through
  stop(msg)
}

cat("--- Pre-flight check passed: All model artifacts found. ---\n")

# 1. LOAD (Once at startup)
final_model <- readRDS("output_production/final_model.rds")
shap_bg     <- readRDS("output_production/shap_background.rds") 
source("scripts_production/00_utils_shap.R")

# Define the levels for Likert scales to ensure ordered factors match
likert_levels <- c("Strongly Disagree", "Somewhat Disagree", "Neither", 
                   "Somewhat Agree", "Strongly Agree", "Refused_Answer", "Not_Applicable")

# Create the dataframe representing a single "Student" user
new_data <- data.frame(
  # --- Demographics ---
  WBINC = factor("Upper Middle", levels = c("Low", "Lower Middle", "Upper Middle", "High")),
  gender = factor("Female", levels = c("Male", "Female")),
  age = 22, # Numeric
  hhsize = 4, # Numeric
  GEMHHINC = factor("Middle Third", levels = c("Lowest Third", "Middle Third", "Upper Third", "Unknown")),
  GEMEDUC = factor("Post-Secondary", levels = c("None", "Some Secondary", "Secondary Degree", "Post-Secondary", "Graduate Experience", "Other/Unknown")),
  cphhinc = factor("No Change", levels = c("Strongly Decrease", "Somewhat Decrease", "No Change", "Somewhat Increase", "Strongly Increase"), ordered = TRUE),
  
  # --- Perceptions ---
  KNOWENyy = factor("At least one", levels = c("None", "At least one")),
  OPPORTyy = factor("Agree", levels = c("Disagree", "Agree", "Unknown")),
  SUSKILyy = factor("Agree", levels = c("Disagree", "Agree", "Unknown")),
  FRFAILyy = factor("Disagree", levels = c("Disagree", "Agree")),
  EASYSTyy = factor("Agree", levels = c("Disagree", "Agree", "Refused", "Unknown")),
  
  # --- Mindset (Ordered Factors) ---
  OPPISMyy = factor("Somewhat Agree", levels = likert_levels, ordered = TRUE),
  PROACTyy = factor("Strongly Agree", levels = likert_levels, ordered = TRUE),
  CREATIVyy = factor("Somewhat Agree", levels = likert_levels, ordered = TRUE),
  VISIONyy = factor("Neither", levels = likert_levels, ordered = TRUE),
  
  # --- Engineering Flags ---
  # Note: Based on your pipeline script, Mindset_Asked used "Asked"/"Not_Asked"
  Mindset_Asked = factor("Asked", levels = c("Asked", "Not_Asked")), 
  age_is_missing = factor("No", levels = c("No", "Yes"))
)

# --- TEST RUN ---
# Assuming you have loaded 'final_model' and 'shap_bg' as per your script:

# 1. Compute
shap_res <- compute_single_shap(final_model, shap_bg, new_data)

# 2. Get the List of Plots
plots <- plot_shap_contribution(shap_res, top_n = 5)

# 3. Save Positive Plot
ggsave("shap_positive.png", plot = plots$positive, width = 6, height = 4, dpi = 300)

# 4. Save Negative Plot
ggsave("shap_negative.png", plot = plots$negative, width = 6, height = 4, dpi = 300)

cat("Saved shap_positive.png and shap_negative.png\n")