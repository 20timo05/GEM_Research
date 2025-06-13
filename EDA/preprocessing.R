# ===================================================================
# --- 2. DATA PREPROCESSING & FEATURE ENGINEERING ---
# ===================================================================

# ===================================================================
# --- Training Objective ---
# Target Variable:
#   An individual's intention to start a business within the next three years (a binary variable).
# Features:
#   Demographics (age, gender, education level), perceptual variables (perceived opportunities, perceived capabilities, fear of failure), and socio-economic factors.
# Verification:
#   Your model's findings on the importance of factors like fear of failure can be compared to the global and national report findings.
# ===================================================================


# Target Variable: FUTSUPNO ("Entrepreneurial intentions (in 18-64 sample that is not involved in entrepreneurial activity)")
# Derived from: FUTSUP("Are you, alone or with others, expecting to start a new business... within the next three years?")

library(dplyr)
library(data.table)

setwd("C:\\Users\\timor\\Desktop\\Hochschule\\Machine_Learning")
gem_data <- fread("data/GEM2021APSGlobalIndividualLevelData_15Feb2023.csv")


model_data <- gem_data %>% filter(!is.na(FUTSUPNO))
