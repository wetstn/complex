# Install and load required packages
install.packages("tidyverse")
install.packages("finalfit")
install.packages("httpuv")
install.packages("googlesheets4")
install.packages("arsenal")
install.packages("dplyr")
install.packages("broom")
library(tidyverse)
library(finalfit)
library(httpuv)
library(googlesheets4)
library(arsenal)
library(dplyr)
library(broom)

# Set your working directory
setwd("C:/Users/westo/OneDrive/Desktop/Complex Closure")

# Download and read full data frame from the shared Google Drive 
full_df<-read_sheet("https://docs.google.com/spreadsheets/d/1_Eir3hYTAjuN0rE8Xu8mI5MvBB6wwg4muboauUkjiVE/edit?usp=sharing", sheet = "full_df_complex")

# Specify the variables we want to change to numeric class
numerics_to_change <- c("age", "preop_weight_lbs", "height_in", "preop_bmi",
                       "hgb", "albumin", "procedure_duration", "levels_incise",
                       "instrumentation_levels", "length_of_closure_cm", 
                       "ebl", "drain_duration_days", "los_days", "target_levels",
                       "days_until_postop_chemo", "days_until_postop_radiation",
                       "fu_period_mo", "number_drains",
                       "asa")

# Change the class of the variables to numeric
full_df <- full_df %>%
  mutate_at(vars(numerics_to_change), as.numeric)

# Specify the variables we want to change to character class
factors_to_change <- c('any_complication_adjusted',
                          "sex", "htn", "hx_cva", "hx_mi", "aca_use", 
                          "hx_chemo", "hx_radiation", "hx_prior_spine",
                          "chronic_steroid",
                          "neoplasm", "t_cervical", "t_thoracic",
                          "t_lumbar", "t_sacral_coccygeal", "lam_hem_lamino",
                          "fac_form", "any_corp", "paraspinous_flap",
                          "latissimus_flap", "trapezius_flap",
                          "thoracolumbar_flap", "gluteal_flap",
                          "rotational_flap", "rectus_flap",
                          "local_advancement", "v_y_flap",
                          "other_flap", "any_instrumentation",
                          "bone_matrix", "non_vanc_abx_infusion",
                          "sup_and_deep",
                          "ssi", "necrosis",
                          "dehiscence", 'dehiscence_without_ssi',
                          "hematoma", "seroma",
                          "csf_leak", "intentional_durotomy_csf_leak",
                          "unintentional_durotomy_csf_leak", "reoperation",
                          "mortality",
                          "in_0_to_10", "in_10_to_20", "in_20_to_30",
                          "in_30_to_40", "in_40_to_50", "in_50_to_60",
                          "in_60_to_70")

full_df <- full_df %>%
  mutate_at(vars(factors_to_change), as.factor)

# Subset trapezius and paraspinous flap closure cohorts 
para_df <- full_df %>%
  filter(paraspinous_flap == "1")

trap_df <- full_df %>%
  filter(trapezius_flap == "1")
#-----------------------------------------------------------
# Defining the Data Summary  
#-----------------------------------------------------------
# Define variables
dependent = "any_complication_adjusted"
explanatory = c("age", "sex", "height_in",
            "preop_weight_lbs", "preop_bmi", "asa",
            "smoker", "dm",
            "htn", "hx_cva",
            "hx_mi", "aca_use", 
            "steroid_use", 
            "hx_chemo", "hx_radiation",
            "radiation_type", 
            "hx_prior_spine",
            "neoplasm", "primary_spinal_tumor",
            "primary_tumor_type",
            "t_cervical", "t_thoracic",
            "t_lumbar", "t_sacral_coccygeal",
            "lam_hem_lamino", "fac_form", "fusion", 
            "any_corp", "discectomy", "costotransversectomy",
            "lateral_extracavitary", "sacrectomy", 
            "spine_procedure_other", 
            "target_levels", 
            "levels_incise", 
            "in_0_to_10", "in_10_to_20", "in_20_to_30",
            "in_30_to_40", "in_40_to_50", "in_50_to_60",
            "in_60_to_70",
            "length_of_closure_cm",
            "instrumentation_levels",
            "any_instrumentation",
            "rods", "plates", "cages", 
            "bone_matrix", 
            "vancomycin_powder", "vancomycin_non_powder",
            "non_vanc_abx_infusion",
            "closure_type", "paraspinous_flap", "latissimus_flap",
            "trapezius_flap", "thoracolumbar_flap", 
            "gluteal_flap", "rotational_flap", 
            "rectus_flap", "local_advancement", 
            "v_y_flap", "other_flap",
            "procedure_duration", "ebl", 
            "drains_placed", "number_drains", "superficial_depth", 
            "deep_drain", "sup_and_deep", 
            "drain_duration_days", "los_days", 
            "fu_period_mo", 
            "postop_chemo", "postop_radiation",
            "ssi", "infection_depth",
            "necrosis", "dehiscence",
            "dehiscence_without_ssi",
            "hematoma", "seroma", "csf_leak",
            "intentional_durotomy_csf_leak",
            "unintentional_durotomy_csf_leak",
            "reoperation","reoperation_rt_wound_complication",
            "mortality")

# Paraspinous flap closure data summary by any complication, adjusted
para_summary <- para_df %>% 
  summary_factorlist(dependent, explanatory,
                     na_include=TRUE, 
                     na_include_dependent = TRUE, 
                     total_col = TRUE, 
                     add_col_totals = TRUE, 
                     col_totals_prefix = "n = ", 
                     p=FALSE, 
                     cont = "mean",
                     cont_cut = 2,
                     orderbytotal = TRUE, 
                     include_row_totals_percent = TRUE,
                     digits = c(1, 1, 3, 1, 0)
  )

# Write the summary data frame to the shared Google Drive
write_sheet(para_summary, ss = "https://docs.google.com/spreadsheets/d/1WHrE_KVeMVXIVz9mKMip50OcOwi6l7AwWz0ikqQxTkU/edit?usp=sharing", sheet = "paraspinous_summary")

# Trapezius flap cohort data summary by any complication, adjusted
trap_summary <- trap_df %>% 
  summary_factorlist(dependent, explanatory,
                     na_include=TRUE, 
                     na_include_dependent = TRUE, 
                     total_col = TRUE, 
                     add_col_totals = TRUE, 
                     col_totals_prefix = "n = ", 
                     p=FALSE, 
                     cont = "mean",
                     cont_cut = 2,
                     orderbytotal = FALSE, 
                     include_row_totals_percent = TRUE,
                     digits = c(1, 1, 3, 1, 0)
  )

# Write the summary data frame to the shared Google Drive
write_sheet(trap_summary, ss = "https://docs.google.com/spreadsheets/d/1WHrE_KVeMVXIVz9mKMip50OcOwi6l7AwWz0ikqQxTkU/edit?usp=sharing", sheet = "trapezius_summary")
#-----------------------------------------------------------
# Student T-test Analysis
#-----------------------------------------------------------
numeric_vars <- c("age", "height_in", "preop_weight_lbs", "preop_bmi", "asa",
                  "target_levels", "levels_incise", "length_of_closure_cm", 
                  "instrumentation_levels", "procedure_duration", 
                  "ebl", "number_drains", "drain_duration_days", "los_days", 
                  "fu_period_mo")

# Paraspinous T-test
# Create an empty data frame to store results
para_t_table <- 
  data.frame(variable = character(), 
             p_value = numeric(), 
             stringsAsFactors = FALSE)

# Loop through each numeric variable
for (numeric_var in numeric_vars) {
  # Perform t-test
  para_t_test_result <- 
    t.test(get(numeric_var) ~ any_complication_adjusted, 
           data = para_df)
  
  # Extract p-value
  p_value <- para_t_test_result$p.value
  
  # Append results to the data frame
  para_t_table <- 
    rbind(para_t_table, data.frame(variable = numeric_var, 
                                   p_value = p_value))
}

# Write the T-test data frame to the shared Google Drive
write_sheet(para_t_table, ss = "https://docs.google.com/spreadsheets/d/1WHrE_KVeMVXIVz9mKMip50OcOwi6l7AwWz0ikqQxTkU/edit?usp=sharing", sheet = "para_t_table")

# Trapezius T-test
trap_t_table <- 
  data.frame(variable = character(), 
             p_value = numeric(), 
             stringsAsFactors = FALSE)

for (numeric_var in numeric_vars) {
  trap_t_test_result <- 
    t.test(get(numeric_var) ~ any_complication_adjusted, 
           data = trap_df)
  
  p_value <- trap_t_test_result$p.value
  
  trap_t_table <- 
    rbind(trap_t_table, data.frame(variable = numeric_var, 
                                   p_value = p_value))
}

write_sheet(trap_t_table, ss = "https://docs.google.com/spreadsheets/d/1WHrE_KVeMVXIVz9mKMip50OcOwi6l7AwWz0ikqQxTkU/edit?usp=sharing", sheet = "trap_t_table")
#-----------------------------------------------------------
# Chi-Squared Analysis
#-----------------------------------------------------------
categorical_vars <- c("male", "female",
                      "former_smoker", "current_smoker",
                      "niddm", "iddm", "htn", "hx_cva",
                      "hx_mi", "aca_use", 
                      "chronic_steroid", 
                      "hx_chemo", "hx_radiation",
                      "hx_prior_spine",
                      "neoplasm", "primary_spinal_tumor",
                      "brea_tumor", "chor_tumor",
                      "epen_tumor", "lung_tumor", 
                      "rena_tumor", "meta_tumor",
                      "othe_tumor", "schw_tumor", 
                      "pros_tumor", "mult_tumor", 
                      "spin_tumor",
                      "t_cervical", "t_thoracic",
                      "t_lumbar", "t_sacral_coccygeal",
                      "lam_hem_lamino", "fac_form", "fusion", 
                      "any_corp", "discectomy", "costotransversectomy",
                      "lateral_extracavitary", "sacrectomy", 
                      "spine_procedure_other", 
                      "in_10_to_20", "in_20_to_30",
                      "in_30_to_40", "in_40_to_50", "in_50_to_60",
                      "in_60_to_70",
                      "any_instrumentation",
                      "rods", "plates", "cages", 
                      "bone_matrix", 
                      "vancomycin_powder", "vancomycin_non_powder",
                      "non_vanc_abx_infusion",
                      "paraspinous_flap", "latissimus_flap",
                      "trapezius_flap", "thoracolumbar_flap", 
                      "gluteal_flap", "rotational_flap", 
                      "rectus_flap", "local_advancement", 
                      "v_y_flap", "other_flap",
                      "superficial_depth", 
                      "deep_drain", "sup_and_deep", 
                      "postop_chemo", "postop_radiation",
                      "ssi", "deep_ssi", "superficial_ssi",
                      "drain_ssi",
                      "necrosis", "dehiscence",
                      "dehiscence_without_ssi",
                      "hematoma", "seroma", "csf_leak",
                      "intentional_durotomy_csf_leak",
                      "unintentional_durotomy_csf_leak",
                      "reoperation","reoperation_rt_wound_complication",
                      "mortality")

#Paraspinous chi-squared
# Create an empty data frame to store results
para_chi_table <- 
  data.frame(variable = character(), 
             p_value = numeric(), 
             stringsAsFactors = FALSE)

# Loop through each categorical variable
for (categorical_var in categorical_vars) {
  # Perform chi-square test
  chi_square_result <- 
    chisq.test(table(para_df[[categorical_var]], 
                     para_df$any_complication_adjusted))
  
  # Extract p-value
  p_value <- chi_square_result$p.value
  
  # Append results to the data frame
  para_chi_table <- 
    rbind(para_chi_table, data.frame(variable = categorical_var, 
                                p_value = p_value))
}

# Write the chi-squared data frame to the shared Google Drive
write_sheet(para_chi_table, ss = "https://docs.google.com/spreadsheets/d/1WHrE_KVeMVXIVz9mKMip50OcOwi6l7AwWz0ikqQxTkU/edit?usp=sharing", sheet = "para_chi_table")

# Trapezius chi-squared
trap_chi_table <- 
  data.frame(variable = character(), 
             p_value = numeric(), 
             stringsAsFactors = FALSE)

for (categorical_var in categorical_vars) {
  chi_square_result <- 
    chisq.test(table(trap_df[[categorical_var]], 
                     trap_df$any_complication_adjusted))
  
    p_value <- chi_square_result$p.value
  
    trap_chi_table <- 
    rbind(trap_chi_table, data.frame(variable = categorical_var, 
                                     p_value = p_value))
}

write_sheet(trap_chi_table, ss = "https://docs.google.com/spreadsheets/d/1WHrE_KVeMVXIVz9mKMip50OcOwi6l7AwWz0ikqQxTkU/edit?usp=sharing", sheet = "trap_chi_table")
#-----------------------------------------------------------
# Regression Models
#-----------------------------------------------------------
# Define the following regression parameters
predictor_vars <- c("in_10_to_20", "in_20_to_30", "in_30_to_40", 
                    "in_40_to_50", "in_50_to_60", "in_60_to_70")
response_vars <- c("ssi", "seroma", "dehiscence", "necrosis", 
                   "csf_leak", "reoperation", "mortality", 
                   "any_complication_adjusted")
#-----------------------------------------------------------
# Paraspinous closure length logistic regression
#-----------------------------------------------------------
# Univariate regression
# Empty data frame to store results
pu_result_df <- data.frame(variable = character(), 
                           response_variable = character(), 
                           p_value = numeric(), 
                           stringsAsFactors = FALSE)

# Loop through each response variable
for (response_var in response_vars) {
  # Loop through each predictor variable
  for (predictor_var in predictor_vars) {
    # Create the formula dynamically
    formula <- as.formula(paste(response_var, "~", predictor_var))
    
    # Fit the logistic regression model
    logistic_model <- glm(formula, data = para_df, 
                          family = "binomial")
    
    # Extract p-value for the predictor variable and store in the data frame
    p_value <- tidy(logistic_model)$p.value[2]  
    # Assuming the predictor variable is the second row
    pu_result_df <- rbind(pu_result_df, data.frame(
      variable = predictor_var, 
      response_variable = response_var, p_value = p_value))
  }
}

# Format and reshape the result
pu_result_df$response_variable <- 
  factor(pu_result_df$response_variable,
         levels = c("ssi", "dehiscence", "seroma", "reoperation", 
                    "mortality", "any_complication_adjusted", 
                    "csf_leak", "necrosis"))

pu_wide_result <- 
  tidyr::spread(pu_result_df, key = response_variable, value = p_value)

# Write the paraspinous univariate regression data frame to the shared Google Drive
write_sheet(pu_wide_result, ss = "https://docs.google.com/spreadsheets/d/1WHrE_KVeMVXIVz9mKMip50OcOwi6l7AwWz0ikqQxTkU/edit?usp=sharing", sheet = "para_uni")

# Multivariate regression
pm_result_df <- data.frame(variable = character(), 
                           response_variable = character(), 
                           p_value = numeric(), 
                           stringsAsFactors = FALSE)

for (response_var in response_vars) {
  for (predictor_var in predictor_vars) {
    formula <- as.formula(paste(response_var, "~", predictor_var,
                                "+ trapezius_flap"))
    logistic_model <- glm(formula, data = para_df, 
                          family = "binomial")
    p_value <- tidy(logistic_model)$p.value[2]
    pm_result_df <- rbind(pm_result_df, data.frame(
      variable = predictor_var, 
      response_variable = response_var, p_value = p_value))
  }
}

pm_result_df$response_variable <- 
  factor(pm_result_df$response_variable,
         levels = c("ssi", "dehiscence", "seroma", "reoperation", 
                    "mortality", "any_complication_adjusted", 
                    "csf_leak", "necrosis"))
pm_wide_result <- 
  tidyr::spread(pm_result_df, key = response_variable, value = p_value)

write_sheet(pm_wide_result, ss = "https://docs.google.com/spreadsheets/d/1WHrE_KVeMVXIVz9mKMip50OcOwi6l7AwWz0ikqQxTkU/edit?usp=sharing", sheet = "para_multi")

#-----------------------------------------------------------
# Trapezius closure length logistic regression
#-----------------------------------------------------------
# Univariate regression
tu_result_df <- data.frame(variable = character(), 
                           response_variable = character(), 
                           p_value = numeric(), 
                           stringsAsFactors = FALSE)

for (response_var in response_vars) {
  for (predictor_var in predictor_vars) {
    formula <- as.formula(paste(response_var, "~", predictor_var))
    logistic_model <- glm(formula, data = trap_df, 
                          family = "binomial")
    p_value <- tidy(logistic_model)$p.value[2]
    tu_result_df <- rbind(tu_result_df, data.frame(
      variable = predictor_var, 
      response_variable = response_var, p_value = p_value))
  }
}

tu_result_df$response_variable <- 
  factor(tu_result_df$response_variable,
         levels = c("ssi", "dehiscence", "seroma", "reoperation", 
                    "mortality", "any_complication_adjusted", 
                    "csf_leak", "necrosis"))
tu_wide_result <- 
  tidyr::spread(tu_result_df, key = response_variable, value = p_value)

write_sheet(tu_wide_result, ss = "https://docs.google.com/spreadsheets/d/1WHrE_KVeMVXIVz9mKMip50OcOwi6l7AwWz0ikqQxTkU/edit?usp=sharing", sheet = "trap_uni")

# Multivariate regression
tm_result_df <- data.frame(variable = character(), 
                           response_variable = character(), 
                           p_value = numeric(), 
                           stringsAsFactors = FALSE)

for (response_var in response_vars) {
  for (predictor_var in predictor_vars) {
    formula <- as.formula(paste(response_var, "~", predictor_var,
                                "+ paraspinous_flap"))
    logistic_model <- glm(formula, data = trap_df, 
                          family = "binomial")
    p_value <- tidy(logistic_model)$p.value[2]
    tm_result_df <- rbind(tm_result_df, data.frame(
      variable = predictor_var, 
      response_variable = response_var, p_value = p_value))
  }
}

tm_result_df$response_variable <- 
  factor(tm_result_df$response_variable,
         levels = c("ssi", "dehiscence", "seroma", "reoperation", 
                    "mortality", "any_complication_adjusted", 
                    "csf_leak", "necrosis"))
tm_wide_result <- 
  tidyr::spread(tm_result_df, key = response_variable, value = p_value)

write_sheet(tm_wide_result, ss = "https://docs.google.com/spreadsheets/d/1WHrE_KVeMVXIVz9mKMip50OcOwi6l7AwWz0ikqQxTkU/edit?usp=sharing", sheet = "trap_multi")

#-----------------------------------------------------------
# Mixed univariate logistic/linear regression quality control
#-----------------------------------------------------------
# Check data type
vars <- c("female", "age", "preop_bmi", 
          "current_smoker", "former_smoker", 
          "htn", "hx_mi", "hx_cva", "niddm", "iddm", "asa",
          "aca_use", "chronic_steroid", "neoplasm", 
          "hx_chemo", "hx_radiation", "hx_prior_spine",
          "target_levels", "t_cervical", "t_thoracic",
          "t_lumbar", "t_sacral_coccygeal",
          "levels_incise", "length_of_closure_cm",
          "instrumentation_levels", "procedure_duration", 
          "ebl", "number_drains", "drain_duration_days", 
          "ssi", "seroma", "dehiscence", "necrosis", 
          "csf_leak", "reoperation", "mortality", 
          "any_complication_adjusted", "los_days")

vars_to_numeric <- c("age", "asa", 'preop_bmi',
                     "target_levels", "levels_incise", 
                     "instrumentation_levels",
                     "length_of_closure_cm", "procedure_duration",
                     "ebl", "number_drains", "drain_duration_days",
                     "los_days")

para_df <- para_df %>%
  mutate_at(vars(vars_to_numeric), as.numeric)

vars_to_factor <- c("female", "current_smoker", "former_smoker",
                    "htn", "hx_mi", "hx_cva", "niddm", "iddm",
                    "aca_use", "chronic_steroid", "neoplasm",
                    "hx_chemo", "hx_radiation", 
                    "hx_prior_spine", "t_cervical", "t_thoracic",
                    "t_lumbar", "t_sacral_coccygeal", 
                    "ssi", "seroma", "dehiscence", "necrosis", 
                    "csf_leak", "reoperation", "mortality",
                    "any_complication_adjusted")

para_df <- para_df %>%
  mutate_at(vars(vars_to_factor), as.factor)

#Confirm mutation of data type
str(para_df[, vars])

#Check for missingness
missing_values <- sapply(para_df[vars_to_numeric], function(x) sum(is.na(x)))

# Display the results
missing_values

#Check for missingness
missing_values <- sapply(para_df[vars_to_factor], function(x) sum(is.na(x)))

# Display the results
missing_values

#-----------------------------------------------------------
# Paraspinous univariate logistic/linear regression
#-----------------------------------------------------------
vars_to_factor <- c("female", "current_smoker", "former_smoker",
                    "htn", "hx_mi", "hx_cva", "niddm", "iddm",
                    "aca_use", "chronic_steroid", "neoplasm",
                    "hx_chemo", "hx_radiation", 
                    "hx_prior_spine", "t_cervical", "t_thoracic",
                    "t_lumbar", "t_sacral_coccygeal", 
                    "ssi", "seroma", "dehiscence", "necrosis", 
                    "csf_leak", "reoperation", "mortality",
                    "any_complication_adjusted")

predictor_vars <- c("female", "age", "preop_bmi",
                    "current_smoker", "former_smoker", 
                    "htn", "hx_mi", "hx_cva","niddm", "iddm", "asa",
                    "aca_use", "chronic_steroid", "neoplasm", 
                    "hx_chemo", "hx_radiation", "hx_prior_spine",
                    "target_levels", "t_cervical", "t_thoracic",
                    "t_lumbar", "t_sacral_coccygeal",
                    "levels_incise", "length_of_closure_cm",
                    "instrumentation_levels", "procedure_duration", 
                    "ebl", "number_drains", "drain_duration_days")

response_vars <- c("ssi", "seroma", "dehiscence", 
                   "csf_leak", "reoperation", "mortality", 
                   "any_complication_adjusted")

pu2_result_df <- data.frame(variable = character(), 
                            response_variable = character(), 
                            p_value = numeric(), 
                            stringsAsFactors = FALSE)

for (response_var in response_vars) {
  for (predictor_var in predictor_vars) {
    formula <- as.formula(paste(response_var, "~", predictor_var))
    
    # Convert response variables to numeric for linear regression
    para_df_linear <- para_df %>%
      mutate_at(vars(vars_to_factor), as.numeric)
    
    if (predictor_var %in% c("age", "asa", "preop_bmi",
                             "target_levels", "levels_incise", 
                             "instrumentation_levels",
                             "length_of_closure_cm", "procedure_duration",
                             "ebl", "number_drains", "drain_duration_days",
                             "los_days")) {
      linear_model <- 
        lm(formula, data = para_df_linear)
      p_value <- summary(linear_model)$coefficients[, "Pr(>|t|)"][2]
      
      # Revert back to factors
      para_df_linear <- para_df_linear %>%
        mutate_at(vars(vars_to_factor), as.factor)
    } else {
      # Convert response variables to factors for logistic regression
      para_df_logistic <- para_df %>%
        mutate_at(vars(vars_to_factor), as.factor)
      
      logistic_model <- 
        glm(formula, data = para_df_logistic, family = "binomial")
      p_value <- tidy(logistic_model)$p.value[2]
    }
    
    pu2_result_df <- rbind(pu2_result_df, data.frame(
      variable = predictor_var, 
      response_variable = response_var, p_value = p_value))
  }
}

pu2_result_df$response_variable <- 
  factor(pu2_result_df$response_variable,
         levels = c("ssi", "dehiscence", "seroma", 
                    "csf_leak", "necrosis", "reoperation", 
                    "mortality", "los_days",
                    "any_complication_adjusted"))

pu2_wide_result <- 
  tidyr::spread(pu2_result_df, 
                key = response_variable, value = p_value)

write_sheet(pu2_wide_result, ss = "https://docs.google.com/spreadsheets/d/1WHrE_KVeMVXIVz9mKMip50OcOwi6l7AwWz0ikqQxTkU/edit?usp=sharing", sheet = "para_uni2")

#-----------------------------------------------------------
# Trapezius univariate logistic/linear regression
#-----------------------------------------------------------
tu2_result_df <- data.frame(variable = character(), 
                            response_variable = character(), 
                            p_value = numeric(), 
                            stringsAsFactors = FALSE)

for (response_var in response_vars) {
  for (predictor_var in predictor_vars) {
    formula <- as.formula(paste(response_var, "~", predictor_var))
    
        trap_df_linear <- trap_df %>%
      mutate_at(vars(vars_to_factor), as.numeric)
    
    if (predictor_var %in% c("age", "asa", "preop_bmi",
                             "target_levels", "levels_incise", 
                             "instrumentation_levels",
                             "length_of_closure_cm", "procedure_duration",
                             "ebl", "number_drains", "drain_duration_days",
                             "los_days")) {
      linear_model <- 
        lm(formula, data = trap_df_linear)
      p_value <- summary(linear_model)$coefficients[, "Pr(>|t|)"][2]
      
      trap_df_linear <- trap_df_linear %>%
        mutate_at(vars(vars_to_factor), as.factor)
    } else {
      
      trap_df_logistic <- trap_df %>%
        mutate_at(vars(vars_to_factor), as.factor)
      
      logistic_model <- 
        glm(formula, data = trap_df_logistic, family = "binomial")
      p_value <- tidy(logistic_model)$p.value[2]
    }
    
    tu2_result_df <- rbind(tu2_result_df, data.frame(
      variable = predictor_var, 
      response_variable = response_var, p_value = p_value))
  }
}

tu2_result_df$response_variable <- 
  factor(tu2_result_df$response_variable,
         levels = c("ssi", "dehiscence", "seroma", 
                    "csf_leak", "necrosis", "reoperation", 
                    "mortality", "los_days",
                    "any_complication_adjusted"))

tu2_wide_result <- 
  tidyr::spread(tu2_result_df, 
                key = response_variable, value = p_value)

write_sheet(tu2_wide_result, ss = "https://docs.google.com/spreadsheets/d/1WHrE_KVeMVXIVz9mKMip50OcOwi6l7AwWz0ikqQxTkU/edit?usp=sharing", sheet = "trap_uni2")

#-----------------------------------------------------------
# Mixed multivariate logistic/linear regression quality control
#-----------------------------------------------------------
#-----------------------------------------------------------
# Paraspinous multivariate logistic/linear regression
#-----------------------------------------------------------

# Spot check with univariate linear regression
linear_model <- lm(as.formula(paste("ssi", "~", "age")), data = para_df)
summary(linear_model)
# Spot check with multivariate linear regression
linear_model <- lm(as.formula(paste("ssi", "~", "age"+"preop_bmi")), data = para_df)
summary(linear_model)
