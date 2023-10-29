# Install and load required packages
install.packages("tidyverse")
install.packages("finalfit")
install.packages("httpuv")
install.packages("googlesheets4")
install.packages("arsenal")
library(tidyverse)
library(finalfit)
library(httpuv)
library(googlesheets4)
library(arsenal)

# Set your working directory
setwd("C:/Users/westo/OneDrive/Desktop/Complex Closure")

# Download and read full data frame from the shared Google Drive 
full_df<-read_sheet("https://docs.google.com/spreadsheets/d/1_Eir3hYTAjuN0rE8Xu8mI5MvBB6wwg4muboauUkjiVE/edit?usp=sharing", sheet = "full_df_complex")

# Specify the variables we want to change to numeric class
columns_to_change <- c("age", "preop_weight_lbs", "height_in", "preop_bmi",
                       "hgb", "albumin", "procedure_duration", "levels_incise",
                       "instrumentation_levels", "length_of_closure_cm", 
                       "ebl", "drain_duration_days", "los_days", "target_levels",
                       "days_until_postop_chemo", "days_until_postop_radiation",
                       "fu_period_mo", "number_drains",
                       "asa")

# Change the class of the variables
full_df <- full_df %>%
  mutate_at(vars(columns_to_change), as.numeric)

# Set dependent and explanatory variables
dependent = "indication"
explanatory = c("age", "sex", "height_in",
                "preop_weight_lbs", "preop_bmi", "asa",
                "smoker", "dm",
                "htn", "hx_cva",
                "hx_mi", "aca_use", 
                "steroid_use", 
                "hx_chemo", "hx_radiation",
                "radiation_type", 
                "hx_prior_spine",
                "neoplasm", "primary_tumor_type",
                "primary_spinal_tumor", "metastatic_tumor",
                "t_cervical", "t_thoracic",
                "t_lumbar", "t_sacral_coccygeal",
                "lam_hem_lamino", "fac_form", "any_corp",
                "discectomy", "fusion", "costotransversectomy",
                "lateral_extracavitary", "sacrectomy", 
                "spine_procedure_other", "target_levels", 
                "levels_incise", "length_of_closure_cm",
                "in_0_to_10", "in_10_to_20", "in_20_to_30",
                "in_30_to_40", "in_40_to_50", "in_50_to_60",
                "in_60_to_70",
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
                "any_complication", "any_complication_adjusted",
                "ssi", "infection_depth", 
                "necrosis", "dehiscence", 
                "dehiscence_without_ssi",
                "hematoma", "seroma", 
                "csf_leak",
                "intentional_durotomy_csf_leak",
                "unintentional_durotomy_csf_leak",
                "reoperation","reoperation_rt_wound_complication",
                "mortality")

# Data summary by indication
full_df_complex_summary <- full_df %>% 
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

# Write the summary data frame locally
# write.csv(full_df_complex_summary, "C:/Users/westo/OneDrive/Desktop/Complex Closure/full_df_complex_summary.csv", row.names=FALSE)

# Write the summary data frame to the shared Google Drive
write_sheet(full_df_complex_summary, ss = "https://docs.google.com/spreadsheets/d/1uVyXahvFenZ34VG0RlcwAboodPXy_EldI6ct2TzOXeo/edit?usp=sharing", sheet = "full_df_complex_summary")