library(fastshap)
library(tidyverse)
library(tidymodels)
library(ranger)

# --- 1. OFFLINE: Background Data Preparation ---
# Run this ONCE when saving your model artifacts.
# It creates the reference dataset that SHAP needs to compare against.
prepare_shap_background <- function(final_workflow, training_data, n_samples = 100, seed = 42) {
  
  # Extract the recipe
  recipe_obj <- extract_recipe(final_workflow)
  
  # Preprocess the training data to get it into the format the model engine expects
  # (converts factors to dummies, handles levels, etc.)
  set.seed(seed)
  background_processed <- bake(recipe_obj, new_data = training_data) %>%
    select(-any_of("FUTSUPNO")) %>% # Remove target
    sample_n(min(nrow(.), n_samples)) # Downsample for speed
  
  return(background_processed)
}


## --- 2. ONLINE: Fast SHAP Calculation ---
compute_single_shap <- function(final_workflow, background_processed, new_observation_raw) {
  
  # Extract necessary components
  model_engine <- extract_fit_engine(final_workflow)
  recipe_obj <- extract_recipe(final_workflow)
  
  # 1. Process ONLY the single new observation
  obs_processed <- bake(recipe_obj, new_data = new_observation_raw) %>%
    select(-any_of("FUTSUPNO"))
  
  # --- FIX: Force both to be standard data.frames ---
  # fastshap throws an error if one is a 'tibble' and the other is a 'data.frame'
  obs_processed_df <- as.data.frame(obs_processed)
  background_processed_df <- as.data.frame(background_processed)
  
  # 2. Define the prediction wrapper for Ranger
  pred_wrapper <- function(object, newdata) {
    # Ranger works best with standard data.frames too
    predict(object, data = newdata)$predictions[, "Yes"]
  }
  
  # 3. Compute SHAP
  shap_values <- fastshap::explain(
    model_engine,
    X = background_processed_df,    # Use the coerced data.frame
    newdata = obs_processed_df,     # Use the coerced data.frame
    pred_wrapper = pred_wrapper,
    nsim = 50,
    adjust = TRUE
  )
  
  # 4. Formatting for visualization
  df_shap <- data.frame(
    feature = colnames(obs_processed_df), 
    shap_value = as.numeric(shap_values), 
    actual_value = as.numeric(obs_processed_df[1, ])
  )
  
  return(df_shap)
}

plot_shap_contribution <- function(shap_df, top_n = 5) {
  
  # 1. Common Data Processing
  processed_df <- shap_df %>%
    mutate(
      # Clean feature names (remove 'yy' and underscores for readability)
      clean_feature = gsub("yy", "", feature),
      # Format Label: "Feature\n(Value)"
      label = paste0(clean_feature, "\n(", round(actual_value, 2), ")"),
      # Calculate absolute impact for sorting and plotting
      abs_val = abs(shap_value)
    )
  
  # Shared Theme settings for consistent look
  common_theme <- theme_minimal() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(size = 11, face = "bold", color = "#404040"),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      axis.title.x = element_text(size = 10, color = "grey30")
    )
  
  # 2. Create POSITIVE Plot (Green)
  pos_data <- processed_df %>%
    filter(shap_value > 0) %>%
    arrange(desc(abs_val)) %>%
    head(top_n)
  
  p_pos <- ggplot(pos_data, aes(x = reorder(label, abs_val), y = abs_val)) +
    geom_col(width = 0.6, fill = "#76D714", color = "black", alpha = 0.9) +
    coord_flip() +
    labs(title = "Positive (+)", x = NULL, y = "Contribution") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + # Nice spacing at right
    common_theme

  # 3. Create NEGATIVE Plot (Red) - NOW ALIGNED LEFT
  neg_data <- processed_df %>%
    filter(shap_value < 0) %>%
    arrange(desc(abs_val)) %>% # Sort by magnitude
    head(top_n)
  
  # We plot 'abs_val' on Y so bars grow to the right
  p_neg <- ggplot(neg_data, aes(x = reorder(label, abs_val), y = abs_val)) +
    geom_col(width = 0.6, fill = "#EE3B3B", color = "black", alpha = 0.9) +
    coord_flip() +
    labs(title = "Negative (-)", x = NULL, y = "Contribution") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    common_theme
  
  # Return a LIST of two plots
  return(list(positive = p_pos, negative = p_neg))
}