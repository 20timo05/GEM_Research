# ==============================================================================
# ANALYSIS: MODELING EXPERIMENTS
# Objective: Compare algorithms (LogReg, RF, XGBoost) to find the best model.
# ==============================================================================

library(tidyverse)
library(tidymodels)
library(themis)       # For SMOTE
library(glmnet)
library(ranger)
library(xgboost)
library(doFuture)     # For Parallel Processing
library(vip)

source("scripts_analysis/00_utils_modeling.R")
source("config/model_configs.R")
set.seed(42)

# ==============================================================================
# 1. Data Splitting & Resampling
# ==============================================================================

student_data <- readRDS("data/processed/student_model_data.rds")

# ==============================================================================
# 2. Data Splitting & Resampling
# ==============================================================================

# Split into Training/Validation set (80%) and a final held-out Test set (20%)
set.seed(42)
data_split <- initial_split(student_data, prop = 0.8, strata = FUTSUPNO)
train_val_data <- training(data_split)
test_data      <- testing(data_split)

# Create CV folds from the ENTIRE training/validation set for tuning.
# This is what tune_bayes will use.
cv_folds <- vfold_cv(train_val_data, v = 5, strata = FUTSUPNO)

cat(sprintf("Data Split Summary:\n - Train/Validation Set: %d rows\n - Test Set: %d rows\n",
            nrow(train_val_data), nrow(test_data)))


# ==============================================================================
# 3. Define Weights (For Class Imbalance)
# ==============================================================================
# Calculate weights using the helper function
weights <- calculate_class_weights(train_val_data, "FUTSUPNO")
xgb_weight_value <- weights$xgb
rf_class_weights <- weights$rf

cat(sprintf("XGBoost Scale Pos Weight: %.2f\n", xgb_weight_value))
cat(sprintf("RF Class Weights: No=%.2f, Yes=%.2f\n", rf_class_weights["No"], rf_class_weights["Yes"]))

all_configs <- get_model_configs()
weights <- calculate_class_weights(train_val_data, "FUTSUPNO")

# ==============================================================================
# 5. Run Experiments Loop
# ==============================================================================

set.seed(42)
bayes_ctrl <- control_bayes(save_pred = TRUE, verbose = FALSE, seed = 42)
metric_set_sens_spec <- metric_set(sens, spec, roc_auc)

cores_to_use <- parallel::detectCores() - 1
registerDoFuture()
plan(multisession, workers = cores_to_use)

output_base_dir <- "output_experiments"

for (experiment_name  in names(all_configs)) {
  
  config <- all_configs[[experiment_name]]

  cat(paste("\n\n================== RUNNING:", experiment_name, "==================\n"))
  
  experiment_output_dir <- file.path(output_base_dir, experiment_name)
  if (!dir.exists(experiment_output_dir)) {
    dir.create(experiment_output_dir, recursive = TRUE)
  }
  
  current_model_spec <- config$model_spec_func(weights)
  current_recipe <- config$recipe_func(train_val_data)
  
  current_workflow <- workflow() %>%
    add_recipe(current_recipe) %>%
    add_model(current_model_spec)

  set.seed(42)
  tune_results <- tune_bayes(
    current_workflow,
    resamples = cv_folds,
    param_info = config$param_grid, # Verwenden Sie das neue param_grid
    initial = 10,                   # Startet mit 10 zufälligen Kombinationen
    iter = 30,                      # Führt 30 intelligente Iterationen durch
    metrics = metric_set_sens_spec,
    control = bayes_ctrl           # Verwendet die vordefinierte Kontrolle
  )
  
  best_params <- select_best(tune_results, metric = "roc_auc")
  final_workflow <- finalize_workflow(current_workflow, best_params)
  
  set.seed(42)
  validation_fit <- last_fit(final_workflow, initial_split(train_val_data, prop = 0.8, strata = FUTSUPNO))
  
  optimal_point <- plot_sensitivity_specificity_tradeoff(
    model_results = validation_fit,
    truth_col = FUTSUPNO,
    prob_col = .pred_Yes,
    output_dir = experiment_output_dir,
    plot_title_suffix = paste0("(", experiment_name, " - Validation)")
  )
  
  evaluate_and_report_validation(
    validation_fit = validation_fit,
    tune_results = tune_results,
    best_params = best_params,
    optimal_threshold = optimal_point$.threshold,
    experiment_name = experiment_name,
    output_dir = experiment_output_dir
  )
}

plan(sequential) # Resets to non-parallel processing
cat("\n\n--- ALL EXPERIMENTS COMPLETE ---\n")