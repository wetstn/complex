#Install and load required packages
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

#Set your working directory
setwd("C:/Users/westo/OneDrive/Desktop/Complex Closure")

#Download and read full data frame from the shared Google Drive 
full_df<-read_sheet("https://docs.google.com/spreadsheets/d/1_Eir3hYTAjuN0rE8Xu8mI5MvBB6wwg4muboauUkjiVE/edit?usp=sharing", sheet = "full_df_complex")

#Set list of parameters to check for missingness
parameters_to_check = c("study_id",
                "indication", "age",
                "sex", "height_in",
                "preop_weight_lbs", "preop_bmi", "asa",
                "smoker", "dm",
                "htn", "hx_cva",
                "hx_mi", "aca_use", 
                "steroid_use", 
                "hx_chemo", "hx_radiation",
                "hx_prior_spine",
                "neoplasm",
                "t_cervical", "t_thoracic",
                "t_lumbar", "t_sacral_coccygeal",
                "lam_hem_lamino", "fac_form", "any_corp",
                "discectomy", "fusion", "costotransversectomy",
                "lateral_extracavitary", "sacrectomy", 
                "spine_procedure_other", "target_levels", 
                "levels_incise", "length_of_closure_cm",
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
                "drains_placed", "los_days", 
                "fu_period_mo",
                "any_complication", "any_complication_sans_int_csf_leak",
                "ssi", "necrosis", "dehiscence",
                "hematoma", "seroma", 
                "csf_leak",
                "intentional_durotomy_csf_leak",
                "unintentional_durotomy_csf_leak",
                "reoperation",
                "mortality")

#Filter full data frame to retain only parameters where missingness is not allowed 
partial_df <- full_df %>%
  select(all_of(parameters_to_check))

#Filter partial data frame for subjects with missing variables
subjects_with_missing_data <- partial_df %>%
  filter(rowSums(is.na(.[parameters_to_check])) > 0)

#Search for patterns of missingness
dependent_parameter = c("study_id")

explanatory_parameters = c("indication", "age",
                        "sex", "height_in",
                        "preop_weight_lbs", "preop_bmi", "asa",
                        "smoker", "dm",
                        "htn", "hx_cva",
                        "hx_mi", "aca_use", 
                        "steroid_use", 
                        "hx_chemo", "hx_radiation",
                        "hx_prior_spine",
                        "neoplasm",
                        "t_cervical", "t_thoracic",
                        "t_lumbar", "t_sacral_coccygeal",
                        "lam_hem_lamino", "fac_form", "any_corp",
                        "discectomy", "fusion", "costotransversectomy",
                        "lateral_extracavitary", "sacrectomy", 
                        "spine_procedure_other", "target_levels", 
                        "levels_incise", "length_of_closure_cm",
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
                        "drains_placed", "los_days", 
                        "fu_period_mo",
                        "any_complication", "any_complication_sans_int_csf_leak",
                        "ssi", "necrosis", "dehiscence",
                        "hematoma", "seroma", 
                        "csf_leak",
                        "intentional_durotomy_csf_leak",
                        "unintentional_durotomy_csf_leak",
                        "reoperation",
                        "mortality")

#Visualize missingness as pattern
subjects_with_missing_data %>% 
  missing_pattern(dependent, explanatory)

#Visualize missingness as plot
subjects_with_missing_data %>% 
  missing_plot(dependent, explanatory)

#Write the subjects_with_missing_data data frame locally
#write.csv(subjects_with_missing_data, "C:/Users/westo/OneDrive/Desktop/Complex Closure/subjects_with_missing_data.csv", row.names=FALSE)

#Write the subjects_with_missing_data data frame to the shared Google Drive
write_sheet(subjects_with_missing_data, ss = "https://docs.google.com/spreadsheets/d/1UvOB3nm-QmoIfO6yrFOBRc1ZQchlQ0j5xPt2ei1MwrE/edit?usp=sharing", sheet = "subjects_with_missing_data")