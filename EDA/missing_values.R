# ===================================================
# --- Explicitly handle missing Values ---
# ===================================================

setwd("C:\\Users\\timor\\Desktop\\Hochschule\\Machine_Learning")
source("EDA\\feature_selection.R")

library(dplyr)
library(scales)

# --- Stage 1: Descriptive Analysis - counting missing values ---
cat("\n--- Descriptive Analysis of Missing Values ---\n")
missing_data <- sapply(model_data, function(x) c(
  absolute = sum(is.na(x)),
  relative = round(sum(is.na(x)) / length(x) * 100, 1)
))

# Convert to a data frame for easier manipulation and sorting
missing_df <- as.data.frame(t(missing_data))
# Ensure 'absolute' column is an integer type
missing_df$absolute <- as.integer(missing_df$absolute)
# Sort by 'absolute' column in descending order
missing_df_sorted <- missing_df[order(-missing_df$absolute), ]
print(missing_df_sorted)

# There are many columns with a high rate of missing values
# Requires careful treatment, because removing results in very high data loss
# Simple Imputation Techniques like Median might be too simple
# and cover up potential information



# --- Stage 2: Hierarchical Clustering ---
# Methodology from official GEM Website:
# "To save interview times and costs, the sets of questions related to attitudes
# and perceptions (four questions each) have been randomly assigned over
# two groups of respondents between 2003 and 2009."

# This means, whole subsets of columns have been randomly withheld from individuals
# = Missing Completely at Random (MCAR) with known mechanism 
# Although our dataset is not from this timespan, we can still suspect that this methodology also applies to 2021.

# Some columns have a high number of Missing Values (see Stage 1 output)
# These are likely clustered into groups (answers to questions that are "back to back" in the Questionaire):



# Generate a Dendogram (Hierarchical Cluster of the missing Values)
# Columns with similar "missingness patterns" will be joined together first.
# The result is a tree showing the order of merging.

# We convert the dataframe into a matrix of TRUE/FALSE where TRUE means NA.
# Then convert to 1s and 0s for distance calculation.
missing_matrix <- model_data %>%
  select(-FUTSUPNO) %>% # Exclude the target variable from the analysis
  is.na()

# Filter out columns that have no missing values at all before clustering
cols_with_missing <-
  colnames(missing_matrix)[colSums(missing_matrix) > 0]
missing_matrix_filtered <- missing_matrix[, cols_with_missing]

col_dist <- dist(t(missing_matrix), method = "euclidean")
hclust_result <- hclust(col_dist, method = "ward.D2")
par(mar = c(5, 1, 4, 8)) # Bottom, Left, Top, Right

plot(
  as.dendrogram(hclust_result),
  horiz = TRUE, # Plot horizontally for readability
  main = "Hierarchical Clustering of Missing Value Patterns",
  xlab = "Dissimilarity (Distance)",
  sub = "Columns with similar missingness are clustered together"
)

cat("\n--- Generating Dendogram for Hierarchical Clustering...  ---\n")

# --- Analysis ---
# Cluster 1: Columns with low missing values counts (middle)
# The cluster WBINC-SUSKILyy have the lowest missing values count (see missing_df_sorted)
# It makes sense that these columns have been merged together,
# but there is no meaningful information here

# Cluster 2: The "Mindset" Block (bottom)
# Branches are very short = 'missingness' pattern is similar
# The questions are a whole block in the Questionaire (Qi11 - Qi14)
mindset_cols <- c("OPPISMyy", "PROACTyy", "CREATIVyy", "VISIONyy")


# Cluster 3: The "Attitudes & Cultural Support" Block (Middle Bottom)
# They form another cluster & also form a block in the Questionaire (Qi15 - Qi18)
# The official Note from the GEM website exlicitly mentioned this group:
# "To save interview times... (see above)"
attitudes_perceptions_cols <- c("CULSUPyy", "EQUALIyy", "NBGOODyy", "NBSTATyy", "NBMEDIyy")
# Note: CULSUPyy is an index derived from the other 4x variables.
# If one of the other 4x columns is missing, then CULSUPyy will be missing.
# Hence, it has the highest absolute missing count of them all (see missing_df_sorted)



# --- Stage 3: Analyze Missingness Blocks ---
# Objective: Determine size of overlap between missing values in the clusters

# Let's numerically check this
analyze_missingness_block <- function(data, cols) {
  # Ensure all specified columns exist in the data to prevent errors
  if (!all(cols %in% names(data))) {
    stop("One or more specified columns are not in the dataframe.")
  }

  summary_table <- data %>%
    # For each row, count how many of the specified columns are NA
    mutate(
      missing_count = rowSums(is.na(select(., all_of(cols))))
    ) %>%
    # Categorize each row based on the count
    mutate(
      missing_status = case_when(
        missing_count == 0 ~ "All Present",
        missing_count == length(cols) ~ "All Missing",
        TRUE ~ "Partially Missing"
      )
    ) %>%
    # Count the number of rows in each category
    count(missing_status) %>%
    # Calculate the percentage for clarity
    mutate(percentage = scales::percent(n / sum(n), accuracy = 0.1))

  return(summary_table)
}


mindset_summary <- analyze_missingness_block(model_data, mindset_cols)
attitudes_summary <- analyze_missingness_block(model_data, attitudes_perceptions_cols)

# --- Print the Results ---
cat("--- Analysis of 'Mindset' Cluster ---\n")
print(mindset_summary)
# --- Analysis ---
# About half the sample (48.7%) was not asked these questions.
# -> split-sample design appears to be roughly 50/50
# Within the group that was asked, the partial non-response rate is significant.
# 12645 / (47538 + 12645) = 21%
# -> Over one-fifth failed to provide a clear answer for all four
# High, but plausible for these abstract mindset questions
# -> Seperate Treatment for All-Missing and Partially Missing

cat("\n--- Analysis of 'Attitudes / Perceptions' Cluster ---\n")
print(attitudes_summary)
# --- Analysis ---
# "All Missing" group is only 35.1%
# -> split-sample design might not be simply 50/50
# Within the group that was asked, the partial non-response rate is enormous
# 44079 / (32045 + 44079) = 58%
# -> This set of Questions seem to be very difficult to answer fully
# Imputating so many rows with an average would potentially introduce
# A LOT of bias into the data.
# -> Remove these columns.

model_data <- model_data %>% select(-any_of(attitudes_perceptions_cols))
cat("Attidues/ Perceptions Columns removed. Remaining columns:", ncol(model_data), "\n")

# --- Stage 4: Handle Missing Values for the 'Mindset' Cluster ---
# Methodology:
# 1. Create a binary flag 'Mindset_Asked' to permanently label rows based
#    on the reason for missingness (split-sample vs. partial non-response).
#    This captures the survey's design.
# 2. Impute the remaining NA values in the mindset columns. We will turn the
#    NAs into an explicit factor level '0' using the modern forcats function.

cat("\n--- Stage 4: Creating 'Mindset_Asked' flag and imputing values ---\n")

# Ensure the forcats library is loaded
if (!require(forcats)) install.packages("forcats")
library(forcats)

# The columns we are targeting
mindset_cols <- c("OPPISMyy", "PROACTyy", "CREATIVyy", "VISIONyy")

model_data <- model_data %>%
  mutate(
    # Step 1: Create the 'Mindset_Asked' flag BEFORE imputation
    Mindset_Asked = case_when(
      rowSums(is.na(select(., all_of(mindset_cols)))) == length(mindset_cols) ~ "Not_Asked",
      TRUE ~ "Asked"
    ),
    Mindset_Asked = as.factor(Mindset_Asked),

    # Step 2: Impute the NA values
    across(
      all_of(mindset_cols),
      ~ fct_na_value_to_level(., level = "Not_Answered")
    )
  )

cat("Flag created and values for Mindset Cluster imputed \n")


# FRFAILOP (= "Fear of failure (in 18-64 sample perceiving good opportunities to start a business)")
# Information captured by:
# FRFAILYY,  ("Fear of Failure"), age & OPPORTyy ("....perceiving good opportunities to start a business")

# column is missing in 61.8% of cases and does not contain a lot of new information
# -> drop the column
model_data <- model_data %>% select(-FRFAILOP)

cat("FRFAILOP column removed due to structural missingness. Remaining columns:", ncol(model_data), "\n")


# --- 1. Handle High/Moderate Missingness Categorical Columns (>5%) ---
# For these, we create a new factor level to treat "missing" as information.
# This applies to: INDSUPyy, EASYSTyy, GEMHHINC, OPPORTyy, SUSKILyy
high_missing_categorical <- c(
  "INDSUPyy", "EASYSTyy", "GEMHHINC", "OPPORTyy", "SUSKILyy"
)

model_data <- model_data %>%
  mutate(
    across(
      all_of(high_missing_categorical),
      ~ fct_na_value_to_level(., level = "Unknown")
    )
  )

cat("Handled high-missingness categorical columns by creating 'Unknown' level.\n")


# --- 2. Handle High/Moderate Missingness Numeric Column: 'age' (5.9%) ---
# We create a binary flag and then impute with the median.
model_data <- model_data %>%
  mutate(
    age_is_missing = as.factor(ifelse(is.na(age), "Yes", "No")),
    age = ifelse(is.na(age), median(age, na.rm = TRUE), age)
  )

cat("Handled 'age' by creating 'age_is_missing' flag and imputing median.\n")


# --- 3. Handle Low Missingness Columns (<5%) ---
# For these, simple median/mode imputation is safe and effective.

# Define the columns
low_missing_numeric <- c("hhsize")
low_missing_categorical <- c(
  "KNOWENyy", "cphhinc", "GEMEDUC", "GEMOCCU", "gender"
)

# Helper function to find the mode (most frequent value)
get_mode <- function(x) {
  ux <- unique(x[!is.na(x)])
  ux[which.max(tabulate(match(x, ux)))]
}

# Impute numeric with median, categorical with mode
model_data <- model_data %>%
  mutate(
    across(
      all_of(low_missing_numeric),
      ~ ifelse(is.na(.), median(., na.rm = TRUE), .)
    ),
    across(
      all_of(low_missing_categorical),
      ~ ifelse(is.na(.), get_mode(.), as.character(.))
    )
  ) %>%
  # The previous step converts factors to characters, so we convert them back
  mutate(across(all_of(low_missing_categorical), as.factor))


cat("Handled low-missingness columns with median/mode imputation.\n")


# --- Final Verification ---
# Check if any missing values remain in the entire dataset.
cat("\n--- Final Verification: Checking for any remaining NAs ---\n")
remaining_nas <- colSums(is.na(model_data))
if (all(remaining_nas == 0)) {
  cat("Success! No missing values remain in the dataset.\n")
} else {
  cat("Warning: Some missing values still exist. Please review:\n")
  print(remaining_nas[remaining_nas > 0])
}