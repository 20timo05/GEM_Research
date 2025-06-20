# ===================================================================
# --- Main Modeling Script (for Experimentation & Validation) ---
# ===================================================================
#
# PURPOSE:
# To build, tune, and evaluate a model to predict student
# entrepreneurial intention (FUTSUPNO). This script uses a train/validation/test
# split to allow for robust model comparison before a final, unbiased evaluation.

# --- 1. SETUP ---
# Load core libraries
library(tidymodels)
library(dplyr)
library(doParallel)
# library(themis) # No longer needed for this experiment

# Load our custom functions for evaluation and plotting
source("./helper_functions.R")

# Load the data (assuming it's created by your EDA script)
source("EDA/eda_students.R")
set.seed(42)

# --- Set the experiment name to keep results organized ---
experiment_name <- "Experiment_RF_ReducedFeatures2"
output_base_dir <- "output" # Base folder for all experiments

# Create full path for this experiment's output
experiment_output_dir <- file.path(output_base_dir, experiment_name)

# Ensure the output directory exists
if (!dir.exists(experiment_output_dir)) {
  dir.create(experiment_output_dir, recursive = TRUE)
  cat(paste0("Created output directory: ", experiment_output_dir, "\n"))
} else {
  cat(paste0("Output directory already exists: ", experiment_output_dir, "\n"))
}


# --- 2. DATA SPLITTING (3-WAY SPLIT) & RESAMPLING ---

# First, split off the final, held-out test set (e.g., 20%)

# ctryalp: MANY categories = much noise. Most categories have very low feature importance
# Mindset_Asked: Based on survey design - not applicable to DIT Startup Campus
student_data <- student_data %>% select(-any_of(c("ctryalp", "Mindset_Asked"))) 
data_split <- initial_split(student_data, prop = 0.80, strata = FUTSUPNO)
test_data  <- testing(data_split)      # This is locked away until the very end
train_val_data <- training(data_split)

# Now, split the remaining data into training and validation sets (e.g., 80/20 split of the 80%)
val_split <- initial_split(train_val_data, prop = 0.80, strata = FUTSUPNO)
train_data <- training(val_split)
validation_data <- testing(val_split)

cat(sprintf("Data Split Summary:\n - Training Set: %d rows\n - Validation Set: %d rows\n - Test Set: %d rows\n",
            nrow(train_data), nrow(validation_data), nrow(test_data)))


# Create cross-validation folds from the TRAINING data for tuning
cv_folds <- vfold_cv(train_data, v = 5, strata = FUTSUPNO)


# --- 3. FEATURE ENGINEERING RECIPE ---
# The recipe is now very simple. The imbalance is handled in the model engine.
my_recipe <-
  recipe(FUTSUPNO ~ ., data = train_data) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors())


# --- 4. MODEL SPECIFICATION & WORKFLOW ---
# Define the model for this experiment.
# We MANUALLY set the class.weights based on the 4-to-1 class ratio.
rf_spec <-
  rand_forest(
    trees = 1000,
    mtry = tune(),
    min_n = tune()
  ) %>%
  set_engine(
    "ranger",
    importance = "impurity",
    class.weights = c("No" = 1.0, "Yes" = 4.0) # <<< THIS IS THE FIX
  ) %>%
  set_mode("classification")

# Combine the recipe and model into a single workflow object
rf_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(rf_spec)


# --- 5. HYPERPARAMETER TUNING ---
# We are now only tuning mtry and min_n. This will be faster.
# Set up parallel processing to speed things up
registerDoParallel(cores = detectCores(logical = FALSE))

# Define the metrics we care about
metric_set_sens_spec <- metric_set(sens, yardstick::spec, roc_auc)

# Tune only mtry and min_n
set.seed(42)
rf_tune_results <- tune_grid(
  rf_workflow,
  resamples = cv_folds,
  grid = 10, # Let tune_grid create a 10-combination grid automatically
  metrics = metric_set_sens_spec,
  control = control_grid(save_pred = TRUE)
)

# Stop the parallel cluster
stopImplicitCluster()

# View the best performing hyperparameter sets, ranked by AUC
show_best(rf_tune_results, metric = "roc_auc")


# --- 6. SELECT AND TRAIN ON VALIDATION SET ---
# Select the best hyperparameters based on CROSS-VALIDATION performance
best_params <- select_best(rf_tune_results, metric = "roc_auc")

# Finalize the workflow with these best parameters
final_workflow <- finalize_workflow(rf_workflow, best_params)

# Train the finalized workflow on the FULL training set and evaluate on the VALIDATION set
set.seed(42)
validation_fit <- last_fit(final_workflow, val_split)


# --- 7. EVALUATE ON VALIDATION SET & GENERATE REPORT ---
# Find the optimal threshold using the VALIDATION set predictions
optimal_point_validation <- plot_sensitivity_specificity_tradeoff(
  model_results = validation_fit,
  truth_col = FUTSUPNO,
  prob_col = .pred_Yes,
  output_dir = experiment_output_dir,
  plot_title_suffix = "(Validation Set)"
)

# Evaluate performance on the VALIDATION set and generate the report
evaluate_and_report_validation(
  validation_fit = validation_fit,
  tune_results = rf_tune_results,
  best_params = best_params,
  optimal_threshold = optimal_point_validation$.threshold,
  experiment_name = experiment_name,
  output_dir = experiment_output_dir
)

# --- END OF EXPERIMENT ---


# List all Features based on Importance
library(vip)
final_ranger_model <- extract_fit_parsnip(validation_fit)
importance_df <- vi(final_ranger_model)
# Print the full list, sorted from most to least important
print(importance_df, n = Inf) # n = Inf shows all rows