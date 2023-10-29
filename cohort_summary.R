# Install and load required packages
install.packages("tidyverse")
install.packages("finalfit")
install.packages("httpuv")
install.packages("googlesheets4")
install.packages("arsenal")
install.packages("dplyr")
library(tidyverse)
library(finalfit)
library(httpuv)
library(googlesheets4)
library(arsenal)
library(dplyr)

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

#-----------------------------------------------------------
# Defining the Data Summary  
#-----------------------------------------------------------
# Set dependent variable
dependent = "any_complication_adjusted"
neo_oth = c("age", "sex", "height_in",
            "preop_weight_lbs", "preop_bmi", "asa",
            "smoker", "dm",
            "htn", "hx_cva",
            "hx_mi", "aca_use", 
            "steroid_use", 
            "hx_chemo", "hx_radiation",
            "radiation_type", 
            "hx_prior_spine",
            "neoplasm", "primary_tumor_type",
            "primary_spinal_tumor",
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
            "drains_placed", "number_drains", "superficial_depth", 
            "deep_drain", "sup_and_deep", 
            "drain_duration_days", "los_days", 
            "fu_period_mo", 
            "postop_chemo", "postop_radiation",
            "ssi",
            "necrosis", "dehiscence",
            "hematoma", "seroma", "csf_leak",
            "intentional_durotomy_csf_leak",
            "unintentional_durotomy_csf_leak",
            "reoperation","reoperation_rt_wound_complication",
            "mortality",
            "in_0_to_10", "in_10_to_20", "in_20_to_30",
            "in_30_to_40", "in_40_to_50", "in_50_to_60",
            "in_60_to_70", "infection_depth",
            "dehiscence_without_ssi")

neo_chi = c("age", "sex", "height_in",
            "preop_weight_lbs", "preop_bmi", "asa",
            "smoker",
            "htn",
            "aca_use", 
            "hx_chemo", "hx_radiation",
            "hx_prior_spine",
            "primary_spinal_tumor",
            "t_cervical", "t_thoracic",
            "t_lumbar", "t_sacral_coccygeal",
            "lam_hem_lamino", "fac_form", "any_corp",
            "fusion", "costotransversectomy",
            "spine_procedure_other", "target_levels", 
            "levels_incise", "length_of_closure_cm",
            "instrumentation_levels",
            "any_instrumentation",
            "rods", "plates", "cages", 
            "bone_matrix", 
            "vancomycin_powder", "vancomycin_non_powder",
            "non_vanc_abx_infusion",
            "paraspinous_flap",
            "trapezius_flap", 
            "procedure_duration", "ebl", 
            "number_drains", "superficial_depth", 
            "sup_and_deep", 
            "drain_duration_days", "los_days", 
            "fu_period_mo", 
            "postop_chemo", "postop_radiation",
            "seroma", 
            "reoperation","reoperation_rt_wound_complication",
            "mortality")

not_neo_exp = c("age", "sex", "height_in",
               "preop_weight_lbs", "preop_bmi", "asa",
               "smoker", "dm",
               "htn", "hx_cva",
               "hx_mi", "aca_use", 
               "steroid_use", 
               "hx_chemo", "hx_radiation",
               "hx_prior_spine",
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
               "drains_placed", "number_drains", "superficial_depth", 
               "deep_drain", "sup_and_deep", 
               "drain_duration_days", "los_days", 
               "fu_period_mo", 
               "ssi", "infection_depth",
               "necrosis", "dehiscence",
               "hematoma", "seroma", 
               "csf_leak",
               "intentional_durotomy_csf_leak",
               "unintentional_durotomy_csf_leak",
               "reoperation","reoperation_rt_wound_complication",
               "mortality",
               "in_0_to_10", "in_10_to_20", "in_20_to_30",
               "in_30_to_40", "in_40_to_50", "in_50_to_60",
               "in_60_to_70", "infection_depth",
               "dehiscence_without_ssi")

deg_chi_exp = c("age", "sex", "height_in",
                "preop_weight_lbs", "preop_bmi", "asa",
                "htn", "aca_use", 
                "hx_prior_spine",
                "t_cervical", "t_thoracic",
                "t_lumbar", "t_sacral_coccygeal",
                "fac_form",
                "spine_procedure_other", "target_levels", 
                "levels_incise", "length_of_closure_cm",
                "instrumentation_levels",
                "cages", 
                "vancomycin_powder", "vancomycin_non_powder",
                "non_vanc_abx_infusion",
                "trapezius_flap",
                "procedure_duration", "ebl", 
                "number_drains", "superficial_depth", 
                "sup_and_deep", 
                "drain_duration_days", "los_days", 
                "fu_period_mo")

con_chi_exp = c("age", "height_in",
                "preop_weight_lbs", "preop_bmi", "asa",
                "target_levels", 
                "levels_incise", "length_of_closure_cm",
                "instrumentation_levels",
                "procedure_duration", "ebl", 
                "number_drains",
                "drain_duration_days", "los_days", 
                "fu_period_mo")

tra_chi_exp = c("age", "height_in",
                "preop_weight_lbs", "preop_bmi", "asa",
                "target_levels", 
                "levels_incise", "length_of_closure_cm",
                "instrumentation_levels",
                "procedure_duration", "ebl", 
                "number_drains",
                "drain_duration_days", "los_days", 
                "fu_period_mo")

def_chi_exp = c("age", "height_in",
                "preop_weight_lbs", "preop_bmi", "asa",
                "htn",
                "target_levels", 
                "levels_incise", "length_of_closure_cm",
                "instrumentation_levels",
                "procedure_duration", "ebl", 
                "number_drains", 
                "drain_duration_days", "los_days", 
                "fu_period_mo")

#-----------------------------------------------------------
# Neoplastic Cohort Summary
#-----------------------------------------------------------

# Subset neoplastic cohort
neo_df <- full_df %>%
  filter(indication == "Neoplastic")

# Neoplastic data summary by any complication
neo_summary <- neo_df %>% 
  summary_factorlist(dependent, neo_oth,
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

neo_chi_summary <- neo_df %>% 
  summary_factorlist(dependent, neo_chi,
                     na_include=TRUE, 
                     na_include_dependent = TRUE, 
                     total_col = TRUE, 
                     add_col_totals = TRUE, 
                     col_totals_prefix = "n = ", 
                     p=TRUE, 
                     cont = "mean",
                     cont_cut = 2,
                     orderbytotal = FALSE, 
                     include_row_totals_percent = TRUE,
                     digits = c(1, 1, 3, 1, 0)
  )

# Write the summary data frame to the shared Google Drive
write_sheet(neo_summary, ss = "https://docs.google.com/spreadsheets/d/1uVyXahvFenZ34VG0RlcwAboodPXy_EldI6ct2TzOXeo/edit?usp=sharing", sheet = "neo_summary")
write_sheet(neo_chi_summary, ss = "https://docs.google.com/spreadsheets/d/1uVyXahvFenZ34VG0RlcwAboodPXy_EldI6ct2TzOXeo/edit?usp=sharing", sheet = "neo_chi_summary")

#-----------------------------------------------------------
# Degenerative Disease Cohort Summary
#-----------------------------------------------------------

# Subset degenerative disease cohort
deg_df <- full_df %>%
  filter(indication == "Degenerative")

# Degenerative disease data summary by any complication (adjusted)
deg_summary <- deg_df %>% 
  summary_factorlist(dependent, not_neo_exp,
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

deg_chi_summary <- deg_df %>% 
  summary_factorlist(dependent, deg_chi_exp,
                     na_include=TRUE, 
                     na_include_dependent = TRUE, 
                     total_col = TRUE, 
                     add_col_totals = TRUE, 
                     col_totals_prefix = "n = ", 
                     p=TRUE, 
                     cont = "mean",
                     cont_cut = 2,
                     orderbytotal = FALSE, 
                     include_row_totals_percent = TRUE,
                     digits = c(1, 1, 3, 1, 0)
  )

# Write the summary data frame to the shared Google Drive
write_sheet(deg_summary, ss = "https://docs.google.com/spreadsheets/d/1uVyXahvFenZ34VG0RlcwAboodPXy_EldI6ct2TzOXeo/edit?usp=sharing", sheet = "deg_summary")
write_sheet(deg_chi_summary, ss = "https://docs.google.com/spreadsheets/d/1uVyXahvFenZ34VG0RlcwAboodPXy_EldI6ct2TzOXeo/edit?usp=sharing", sheet = "deg_chi_summary")

#-----------------------------------------------------------
# Congenital Anomaly Cohort Summary
#-----------------------------------------------------------
# Subset congenital anomaly cohort
con_df <- full_df %>%
  filter(indication == "Congenital Anomaly")

# Congenital anomaly data summary by any complication (adjusted)
con_summary <- con_df %>% 
  summary_factorlist(dependent, not_neo_exp,
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

con_chi_summary <- con_df %>% 
  summary_factorlist(dependent, con_chi_exp,
                     na_include=TRUE, 
                     na_include_dependent = TRUE, 
                     total_col = TRUE, 
                     add_col_totals = TRUE, 
                     col_totals_prefix = "n = ", 
                     p=TRUE, 
                     cont = "mean",
                     cont_cut = 2,
                     orderbytotal = FALSE, 
                     include_row_totals_percent = TRUE,
                     digits = c(1, 1, 3, 1, 0)
  )

# Write the summary data frame to the shared Google Drive
write_sheet(con_summary, ss = "https://docs.google.com/spreadsheets/d/1uVyXahvFenZ34VG0RlcwAboodPXy_EldI6ct2TzOXeo/edit?usp=sharing", sheet = "con_summary")
write_sheet(con_chi_summary, ss = "https://docs.google.com/spreadsheets/d/1uVyXahvFenZ34VG0RlcwAboodPXy_EldI6ct2TzOXeo/edit?usp=sharing", sheet = "con_chi_summary")

#-----------------------------------------------------------
# Trauma Cohort Summary
#-----------------------------------------------------------
# Subset trauma cohort
tra_df <- full_df %>%
  filter(indication == "Trauma")

# Trauma data summary by any complication (adjusted)
tra_summary <- tra_df %>% 
  summary_factorlist(dependent, not_neo_exp,
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

tra_chi_summary <- tra_df %>% 
  summary_factorlist(dependent, tra_chi_exp,
                     na_include=TRUE, 
                     na_include_dependent = TRUE, 
                     total_col = TRUE, 
                     add_col_totals = TRUE, 
                     col_totals_prefix = "n = ", 
                     p=TRUE, 
                     cont = "mean",
                     cont_cut = 2,
                     orderbytotal = FALSE, 
                     include_row_totals_percent = TRUE,
                     digits = c(1, 1, 3, 1, 0)
  )

# Write the summary data frame to the shared Google Drive
write_sheet(tra_summary, ss = "https://docs.google.com/spreadsheets/d/1uVyXahvFenZ34VG0RlcwAboodPXy_EldI6ct2TzOXeo/edit?usp=sharing", sheet = "tra_summary")
write_sheet(tra_chi_summary, ss = "https://docs.google.com/spreadsheets/d/1uVyXahvFenZ34VG0RlcwAboodPXy_EldI6ct2TzOXeo/edit?usp=sharing", sheet = "tra_chi_summary")

#-----------------------------------------------------------
# Deformity Cohort Summary
#-----------------------------------------------------------
# Subset deformity cohort
def_df <- full_df %>%
  filter(indication == "Deformity")

# Deformity data summary by any complication (adjusted)
def_summary <- def_df %>% 
  summary_factorlist(dependent, not_neo_exp,
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

def_chi_summary <- def_df %>% 
  summary_factorlist(dependent, def_chi_exp,
                     na_include=TRUE, 
                     na_include_dependent = TRUE, 
                     total_col = TRUE, 
                     add_col_totals = TRUE, 
                     col_totals_prefix = "n = ", 
                     p=TRUE, 
                     cont = "mean",
                     cont_cut = 2,
                     orderbytotal = FALSE, 
                     include_row_totals_percent = TRUE,
                     digits = c(1, 1, 3, 1, 0)
  )

# Write the summary data frame to the shared Google Drive
write_sheet(def_summary, ss = "https://docs.google.com/spreadsheets/d/1uVyXahvFenZ34VG0RlcwAboodPXy_EldI6ct2TzOXeo/edit?usp=sharing", sheet = "def_summary")
write_sheet(def_chi_summary, ss = "https://docs.google.com/spreadsheets/d/1uVyXahvFenZ34VG0RlcwAboodPXy_EldI6ct2TzOXeo/edit?usp=sharing", sheet = "def_chi_summary")