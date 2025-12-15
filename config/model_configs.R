# config/model_configs.R
# Single source of truth for all model experiment configurations

# Load necessary model packages
library(tidymodels)
library(glmnet)
library(ranger)
library(xgboost)
library(themis)

get_model_configs <- function() {
  
  configs <- list(
    
    "Experiment_1_LogReg_Baseline" = list(
      model_spec_func = function(weights) {
        logistic_reg(penalty = tune(), mixture = 1) %>%
          set_engine("glmnet") %>% set_mode("classification")
      },
      recipe_func = function(data) {
        recipe(FUTSUPNO ~ ., data = data) %>%
          step_dummy(all_nominal_predictors()) %>%
          step_zv(all_predictors())
      },
      param_grid = parameters(penalty())
    ),
    
    "Experiment_2_LogReg_SMOTE" = list(
      model_spec_func = function(weights) {
        logistic_reg(penalty = tune(), mixture = 1) %>%
          set_engine("glmnet") %>% set_mode("classification")
      },
      recipe_func = function(data) {
        recipe(FUTSUPNO ~ ., data = data) %>%
          step_dummy(all_nominal_predictors()) %>%
          step_zv(all_predictors()) %>%
          step_smote(FUTSUPNO, over_ratio = 0.8)
      },
      param_grid = parameters(penalty())
    ),
    
    "Experiment_3_LogReg_Splines" = list(
      model_spec_func = function(weights) {
        logistic_reg(penalty = tune(), mixture = 1) %>%
          set_engine("glmnet") %>% set_mode("classification")
      },
      recipe_func = function(data) {
        recipe(FUTSUPNO ~ ., data = data) %>%
          step_dummy(all_nominal_predictors()) %>%
          step_zv(all_predictors()) %>%
          step_ns(age, deg_free = tune()) %>%
          step_smote(FUTSUPNO, over_ratio = 0.8)
      },
      param_grid = parameters(penalty(), deg_free(range = c(2L, 5L)))
    ),
    
    "Experiment_4_RF_Baseline" = list(
      model_spec_func = function(weights) {
        rand_forest(mtry = tune(), min_n = tune()) %>%
          set_engine("ranger", importance = "permutation") %>%
          set_mode("classification")
      },
      recipe_func = function(data) {
        recipe(FUTSUPNO ~ ., data = data) %>%
          step_dummy(all_nominal_predictors()) %>%
          step_zv(all_predictors())
      },
      param_grid = parameters(mtry(range = c(2, 25)), min_n(range = c(2, 20)))
    ),
    
    "Experiment_5_RF_SMOTE" = list(
      model_spec_func = function(weights) {
        rand_forest(mtry = tune(), min_n = tune()) %>%
          set_engine("ranger", importance = "permutation") %>%
          set_mode("classification")
      },
      recipe_func = function(data) {
        recipe(FUTSUPNO ~ ., data = data) %>%
          step_dummy(all_nominal_predictors()) %>%
          step_zv(all_predictors()) %>%
          step_smote(FUTSUPNO, over_ratio = 0.8)
      },
      param_grid = parameters(mtry(range = c(2, 25)), min_n(range = c(2, 20)))
    ),
    
    "Experiment_6_RF_ManualWeights" = list(
      model_spec_func = function(weights) {
        rand_forest(mtry = tune(), min_n = tune()) %>%
          set_engine(
            "ranger",
            importance = "permutation",
            class.weights = !!weights$rf # Changed to use the weights argument
          ) %>%
          set_mode("classification")
      },
      recipe_func = function(data) {
        recipe(FUTSUPNO ~ ., data = data) %>%
          step_dummy(all_nominal_predictors()) %>%
          step_zv(all_predictors())
      },
      param_grid = parameters(mtry(range = c(2, 25)), min_n(range = c(2, 20)))
    ),
    
    "Experiment_7_XGBoost_Baseline" = list(
      model_spec_func = function(weights) {
        boost_tree(
          trees = tune(), min_n = tune(), tree_depth = tune(), learn_rate = tune()
        ) %>%
          set_engine("xgboost") %>% set_mode("classification")
      },
      recipe_func = function(data) {
        recipe(FUTSUPNO ~ ., data = data) %>%
          step_dummy(all_nominal_predictors()) %>%
          step_zv(all_predictors())
      },
      param_grid = parameters(
        trees(range = c(100, 1000)), min_n(), tree_depth(), learn_rate()
      )
    ),
    
    "Experiment_8_XGBoost_SMOTE" = list(
      model_spec_func = function(weights) {
        boost_tree(
          trees = tune(), min_n = tune(), tree_depth = tune(), learn_rate = tune()
        ) %>%
          set_engine("xgboost") %>% set_mode("classification")
      },
      recipe_func = function(data) {
        recipe(FUTSUPNO ~ ., data = data) %>%
          step_dummy(all_nominal_predictors()) %>%
          step_zv(all_predictors()) %>%
          step_smote(FUTSUPNO, over_ratio = 0.8)
      },
      param_grid = parameters(
        trees(range = c(100, 1000)), min_n(), tree_depth(), learn_rate()
      )
    ),
    
    "Experiment_9_XGBoost_ManualWeights" = list(
      model_spec_func = function(weights) {
        boost_tree(
          trees = tune(), min_n = tune(), tree_depth = tune(), learn_rate = tune()
        ) %>%
          set_engine("xgboost", scale_pos_weight = !!weights$xgb) %>% # Changed to use the weights argument
          set_mode("classification")
      },
      recipe_func = function(data) {
        recipe(FUTSUPNO ~ ., data = data) %>%
          step_dummy(all_nominal_predictors()) %>%
          step_zv(all_predictors())
      },
      param_grid = parameters(
        trees(range = c(100, 1000)), min_n(), tree_depth(), learn_rate()
      )
    )
  )
  
  return(configs)
}