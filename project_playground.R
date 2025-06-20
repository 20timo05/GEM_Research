# --- 1. SETUP: Load Libraries & Data ---
# For the core modeling framework
if (!require(tidymodels)) install.packages("tidymodels")
library(tidymodels)
# For data manipulation
if (!require(dplyr)) install.packages("dplyr")
library(dplyr)
# For plotting
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)
# For variable importance plots
if (!require(vip)) install.packages("vip")
library(vip)
# For tidying model outputs
if (!require(broom)) install.packages("broom")
library(broom)
# For parallel processing to speed up tuning (Random Forest)
if (!require(doParallel)) install.packages("doParallel")
library(doParallel)
library(future)
library(themis) # For SMOTE



# Set the working directory and load the student-specific data
setwd("C:\\Users\\timor\\Desktop\\Hochschule\\Machine_Learning")
# The eda_students.R script is sourced to ensure the 'student_data' object
# is created and available in the environment.
source("EDA/eda_students.R")

# --- Further Feature Reduction ---
# because only ~6000 rows remain, ctryalp is dropped due to Curse Of Dimensionality
# WBINC is kept as a Proxy for ctryalp
student_data <- student_data %>% select(-any_of(c("ctryalp")))

# --- 2. DATA SPLITTING & FOLDS ---
set.seed(42)
data_split <- initial_split(student_data, prop = 0.80, strata = FUTSUPNO)
train_data <- training(data_split)
test_data <- testing(data_split)

# Create cross-validation folds for tuning
set.seed(42)
cv_folds <- vfold_cv(train_data, v = 5, strata = FUTSUPNO)


# --- 3. RECIPE (WITHOUT SMOTE) ---
# The recipe should ONLY contain preprocessing steps that are safe to
# apply to *any* data (training, validation, or test).
# SMOTE is applied later, inside the training loop of the tuning process.
my_recipe <-
  recipe(FUTSUPNO ~ ., data = train_data) %>%
  # Convert all nominal (non-ordered) predictors to dummy variables
  step_dummy(all_nominal_predictors()) %>%
  # Remove variables that have zero variance (e.g., all one value)
  step_zv(all_predictors()) %>%
  # THE KEY CHANGE: Apply SMOTE here. It will only be applied to the
  # training data within each cross-validation fold during tuning.
  step_smote(FUTSUPNO, over_ratio = 0.8, skip = TRUE) # skip = TRUE is important for tune_grid

# --- 4. MODEL SPECIFICATION (FOR TUNING) ---
# We will tune the 'mtry' and 'min_n' hyperparameters.
rf_spec <-
  rand_forest(
    trees = 1000,
    mtry = tune(),
    min_n = tune()
  ) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")


# --- 5. WORKFLOW ---
rf_workflow <-
  workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(rf_spec)


# --- 6. HYPERPARAMETER TUNING ---
# Set up parallel processing
all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)

# Tune the model across the CV folds
# The recipe (including SMOTE) is applied to the training portion of each fold
set.seed(42)
rf_tune_results <- tune_grid(
  rf_workflow,
  resamples = cv_folds,
  grid = 20, # Let tidymodels try 20 different combinations of mtry/min_n
  metrics = metric_set(roc_auc, accuracy, kap), # Focus on roc_auc or kap
  control = control_grid(save_pred = TRUE)
)

# Stop parallel processing
stopImplicitCluster()

# --- 7. ANALYZE TUNING RESULTS & FINALIZE MODEL ---
# Show the best performing models
show_best(rf_tune_results, metric = "kap")
show_best(rf_tune_results, metric = "roc_auc")

# Select the best hyperparameters based on Kappa or ROC AUC
best_params <- select_best(rf_tune_results, metric = "kap")

# Finalize the workflow with the best parameters
final_workflow <- finalize_workflow(rf_workflow, best_params)


# --- 8. FINAL FIT & EVALUATION ON TEST SET ---
# Use last_fit() for the final evaluation.
# It trains the final model on ALL training data and evaluates on the test set.
# This is the canonical way to get a final, unbiased performance estimate.
set.seed(42)
final_fit <- last_fit(final_workflow, data_split)

# Collect and view the test set metrics
# This is the unbiased performance of your final model.
collect_metrics(final_fit)

# Collect the test set predictions to build a confusion matrix
test_predictions <- collect_predictions(final_fit)
conf_mat(test_predictions, truth = FUTSUPNO, estimate = .pred_class)

# You can also plot variable importance from the final fitted model
# The model is inside the .workflow column
final_fit$.workflow[[1]] %>%
  extract_fit_parsnip() %>%
  vip(num_features = 20)