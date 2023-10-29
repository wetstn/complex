# REDCap -> export All Data (all records and fields) 
    # Export as "CSV / Microsoft Excel (labels)"
    # Select "Remove All Identifier Fields"
    # Select "Shift all dates by value between 0 and 364 days"

# Rename this file "ComplexBackClosuresW_DATA_LABELS.csv" before running this script

# Install and load required packages
install.packages("tidyverse")
install.packages("dplyr")
install.packages("httpuv")
install.packages("googlesheets4")
library(tidyverse)
library(dplyr)
library(httpuv)
library(googlesheets4)

# Set your working directory
setwd("C:/Users/westo/OneDrive/Desktop/Complex Closure")

# Download and read hx_ct_disease and header_names from shared Google Drive
header_names<-read_sheet("https://docs.google.com/spreadsheets/d/1p6nJ1Ql1yvYPD3bz4KmGPwG65OSHNRNTkmYWLyyBOLo/edit?usp=sharing")
hx_ct_disease<-read_sheet("https://docs.google.com/spreadsheets/d/17ZpvtXeC0YHck3_2tty0hf65WnZ6iHuDwYr8r_Buwns/edit?usp=sharing")

# Read REDCap data frame
df<-read.csv("ComplexBackClosuresW_DATA_LABELS.csv")

# Set the header names we'll be using for this data frame
df <- df %>% 
  setNames(names(header_names))

# Data cleaning
# Look at only first instance and retain demographics form
filtered_df <- df %>%
  filter(is.na(rpt_instance) | rpt_instance <= 1)

# Group by study_id
filtered_df <- filtered_df %>%
  group_by(study_id) %>%
  summarize_all(~ paste(na.omit(.), collapse = ""))

# Assure the age range is a numeric class
age_data_type <- c("age")
filtered_df <- filtered_df %>%
  mutate_at(vars(age_data_type), as.numeric)

# Filter by age > 18 y.o. 
filtered_df <- filtered_df %>%
  filter(age > 18)

# Filter by inclusion criteria
clean_df <- filtered_df %>%
  filter(open_post == "Yes" & plastics == "Yes" &
           primary == "Yes")

# Elaborate the data frame with extra variables of interest
# Binary IF laminectomy, hemilaminectomy, and/or laminotomy
detailed_df <- clean_df %>%
  mutate(
    lam_hem_lamino = if_else(
      laminectomy == "Checked" | hemilaminectomy == "Checked" |
        laminotomy == "Checked", 1, 0
    )
  )

# Binary IF facetectomy and/or foraminotomy 
detailed_df <- detailed_df %>%
  mutate(
    fac_form = if_else(
      foraminotomy == "Checked" | facetectomy == "Checked",
      1, 0
    )
  )

# Binary IF any corpectomy 
detailed_df <- detailed_df %>%
  mutate(
    any_corp = if_else(
      vertebrectomy_corpectomy == "Checked" | 
        transpedicular_corpectomy == "Checked",
      1, 0
    )
  )

# Binary IF target pathology is cervical, thoracic, lumbar, and sacral and/or coccygeal
detailed_df <- detailed_df %>%
  mutate(
    t_cervical = if_else(
      target_c1 == "Checked" | target_c2 == "Checked" |
        target_c3 == "Checked" | target_c4 == "Checked" | 
        target_c5 == "Checked" | target_c6 == "Checked" |
        target_c7 == "Checked", 1, 0
    )
  )

detailed_df <- detailed_df %>%
  mutate(
    t_thoracic = if_else(
      target_t1 == "Checked" | target_t2 == "Checked" |
        target_t3 == "Checked" | target_t4 == "Checked" | 
        target_t5 == "Checked" | target_t6 == "Checked" |
        target_t7 == "Checked" | target_t8 == "Checked" |
        target_t9 == "Checked" | target_t10 == "Checked" |
        target_t11 == "Checked" | target_t12 == "Checked",
      1, 0
    )
  )

detailed_df <- detailed_df %>%
  mutate(
    t_lumbar = if_else(
      target_l1 == "Checked" | target_l2 == "Checked" |
        target_l3 == "Checked" | target_l4 == "Checked" | 
        target_l5 == "Checked", 1, 0
    )
  )

detailed_df <- detailed_df %>%
  mutate(
    t_sacral_coccygeal  = if_else(
      target_s1 == "Checked" | target_s2 == "Checked" |
        target_s3 == "Checked" | target_s4 == "Checked" | 
        target_s5 == "Checked" | target_coccyx == "Checked", 
      1, 0
    )
  )

# Compute number of levels of target pathology
detailed_df <- detailed_df %>%
  mutate(target_levels = 
           rowSums(across(all_of(c("target_c1", "target_c2", 
                                   "target_c3", "target_c4",
                                   "target_c5", "target_c6", 
                                   "target_c7",
                                   "target_t1", "target_t2", 
                                   "target_t3", "target_t4",
                                   "target_t5", "target_t6", 
                                   "target_t7", "target_t8", 
                                   "target_t9", "target_t10", 
                                   "target_t11", "target_t12",
                                   "target_l1", "target_l2", 
                                   "target_l3", "target_l4",
                                   "target_l5", 
                                   "target_s1", "target_s2", 
                                   "target_s3", "target_s4",
                                   "target_s5", "target_coccyx" 
                        )), ~ . == "Checked")))

# Binary IF incised level is cervical, thoracic, lumbar, and sacral and/or coccygeal
detailed_df <- detailed_df %>%
  mutate(
    i_cervical = if_else(
      incised_c1 == "Checked" | incised_c2 == "Checked" |
        incised_c3 == "Checked" | incised_c4 == "Checked" | 
        incised_c5 == "Checked" | incised_c6 == "Checked" |
        incised_c7 == "Checked", 1, 0
    )
  )

detailed_df <- detailed_df %>%
  mutate(
    i_thoracic = if_else(
      incised_t1 == "Checked" | incised_t2 == "Checked" |
        incised_t3 == "Checked" | incised_t4 == "Checked" | 
        incised_t5 == "Checked" | incised_t6 == "Checked" |
        incised_t7 == "Checked" | incised_t8 == "Checked" |
        incised_t9 == "Checked" | incised_t10 == "Checked" |
        incised_t11 == "Checked" | incised_t12 == "Checked",
      1, 0
    )
  )

detailed_df <- detailed_df %>%
  mutate(
    i_lumbar = if_else(
      incised_l1 == "Checked" | incised_l2 == "Checked" |
        incised_l3 == "Checked" | incised_l4 == "Checked" | 
        incised_l5 == "Checked", 1, 0
    )
  )

detailed_df <- detailed_df %>%
  mutate(
    i_sacral_coccygeal  = if_else(
      incised_s1 == "Checked" | incised_s2 == "Checked" |
        incised_s3 == "Checked" | incised_s4 == "Checked" | 
        incised_s5 == "Checked" | incised_coccyx == "Checked", 
      1, 0
    )
  )

# Yes/No IF instrumented level is cervical, thoracic, lumbar, and sacral and/or coccygeal  
detailed_df <- detailed_df %>%
  mutate(
    in_cervical = if_else(
      instrumentation_c1 == "Checked" | instrumentation_c2 == "Checked" |
        instrumentation_c3 == "Checked" | instrumentation_c4 == "Checked" | 
        instrumentation_c5 == "Checked" | instrumentation_c6 == "Checked" |
        instrumentation_c7 == "Checked", 1, 0
    )
  )

detailed_df <- detailed_df %>%
  mutate(
    in_thoracic = if_else(
      instrumentation_t1 == "Checked" | instrumentation_t2 == "Checked" |
        instrumentation_t3 == "Checked" | instrumentation_t4 == "Checked" | 
        instrumentation_t5 == "Checked" | instrumentation_t6 == "Checked" |
        instrumentation_t7 == "Checked" | instrumentation_t8 == "Checked" |
        instrumentation_t9 == "Checked" | instrumentation_t10 == "Checked" |
        instrumentation_t11 == "Checked" | instrumentation_t12 == "Checked",
      1, 0
    )
  )

detailed_df <- detailed_df %>%
  mutate(
    in_lumbar = if_else(
      instrumentation_l1 == "Checked" | instrumentation_l2 == "Checked" |
        instrumentation_l3 == "Checked" | instrumentation_l4 == "Checked" | 
        instrumentation_l5 == "Checked", 1, 0
    )
  )

detailed_df <- detailed_df %>%
  mutate(
    in_sacral_coccygeal  = if_else(
      instrumentation_s1 == "Checked" | instrumentation_s2 == "Checked" |
        instrumentation_s3 == "Checked" | instrumentation_s4 == "Checked" | 
        instrumentation_s5 == "Checked" | instrumentation_coccyx == "Checked", 
      1, 0
    )
  )

# Binary IF any instrumentation  
detailed_df <- detailed_df %>%
  mutate(
    any_instrumentation = if_else(
      screws == "Checked" | rods == "Checked" |
        plates == "Checked" | cages == "Checked",
      1, 0
    )
  )

# Binary IF use of demineralized bone matrix  
detailed_df <- detailed_df %>%
  mutate(
    bone_matrix = if_else(
      morcelized_allograft == "Checked" | 
        morcelized_autograft == "Checked",
      1, 0
    )
  )

# Binary IF use of bone strut  
detailed_df <- detailed_df %>%
  mutate(
    bone_strut = if_else(
      allograft_strut == "Checked" | 
        autograft_strut == "Checked",
      1, 0
    )
  )

# Yes/No IF intentional durotomy 
detailed_df <- detailed_df %>%
  mutate(
    intentional_durotomy = if_else(
      durotomy == "Yes - intentional (e.g., for intradural lesion)",
      1, 0
    )
  )

detailed_df <- detailed_df %>%
  mutate(
    intentional_durotomy_csf_leak = if_else(
      durotomy == "Yes - intentional (e.g., for intradural lesion)" &
        csf_leak == "Yes",
      1, 0
    )
  )

detailed_df <- detailed_df %>%
  mutate(
    unintentional_durotomy = if_else(
      durotomy == "Yes - accidental (e.g., intra-op complication)",
      1, 0
    )
  )

detailed_df <- detailed_df %>%
  mutate(
    unintentional_durotomy_csf_leak = if_else(
      durotomy == "Yes - accidental (e.g., intra-op complication)" &
        csf_leak == "Yes",
      1, 0
    )
  )

# Binary IF non-vancomycin antibiotic infusion 
detailed_df <- detailed_df %>%
  mutate(
    non_vanc_abx_infusion = if_else(
      ancef == "Checked" | augmentin == "Checked" |
        bactrim == "Checked" | ceftriaxone == "Checked" |
        clindamycin == "Checked" | keflex == "Checked" |
        unasyn == "Checked" | abx_other == "Checked",
      1, 0
    )
  )

# Binary IF superficial AND deep drains were placed 
detailed_df <- detailed_df %>%
  mutate(
    sup_and_deep = if_else(
      superficial_depth == "Checked" & 
        deep_drain == "Checked",
      1, 0
    )
  )

# Binary if dehiscence occurred WITHOUT surgical site infection
detailed_df <- detailed_df %>%
  mutate(dehiscence_without_ssi = ifelse(dehiscence== "Yes" & ssi== "No", 1, 0))

# Binary IF any wound complication occurred 
detailed_df <- detailed_df %>%
  mutate(
    any_complication = if_else(
      ssi == "Yes" | necrosis == "Yes" |
        dehiscence == "Yes" | hematoma == "Yes" |
        seroma == "Yes" | csf_leak == "Yes",
      1, 0
    )
  )

detailed_df <- detailed_df %>%
  mutate(
    any_complication_adjusted = if_else(
      ssi == "Yes" | necrosis == "Yes" |
        dehiscence_without_ssi == 1 | hematoma == "Yes" |
        seroma == "Yes" | unintentional_durotomy_csf_leak == 1,
      1, 0
    )
  )

# Binary if the spinal neoplasm is primary
detailed_df <- detailed_df %>%
  mutate(
    primary_spinal_tumor = case_when(
      primary_tumor_type %in% c("Schwannoma", "Chordoma", 
                                "Spinal Cord Tumor", "Ependymoma") |
        other_tumor_type %in% c("Sacral germ cell tumor", "osteoblastoma", 
                                "Hemangioendothelioma", "Sacral hemangioma",
                                "cancer of connective and soft tissue of thorax, endotheliosarcoma",
                                "Giant Cell Tumor", "chondral blastoma",
                                "Osteogenic sarcoma", "Lipoma", 
                                "Fibrous dysplasia (or aneurysmal bone cyst) of 11th rib head and pedicle",
                                "Pleomorphic sarcoma of bone", 
                                "odontogenic lipomatous paraspinal tumor", 
                                "Mesenchymal chondrosarcoma",
                                "calcified synovial cyst", 
                                "hemangioma", "lipoma", "chondrosarcoma",
                                "ganglioneuroma", "Giant cell sarcoma")
      ~ "Yes",
      TRUE ~ "No"
    )
  )

detailed_df <- detailed_df %>%
  mutate(
    metastatic_tumor = case_when(
      primary_tumor_type %in% c("Metastatic Neoplasm", "Lung Cancer", 
                                "Breast Cancer", "Renal Cell Carcinoma",
                                "Multiple Myeloma") |
        other_tumor_type %in% c("Endometrial", "Carcinoid of Appendix", 
                                "Non-Hodgkins lymphoma", "Esophageal",
                                "thyroid cancer metastatic to the spine",
                                "Liver cancer", "thyroid and pancreas",
                                "bile duct cancer", "Malignant Lymphoma", 
                                "urothelial carcinoma",
                                "colon cancer")
      ~ "Yes",
      TRUE ~ "No"
    )
  )

# Binary if the incision closure length falls into length subgroups
detailed_df <- detailed_df %>%
  mutate(in_0_to_10 = ifelse(length_of_closure_cm>= 0 & length_of_closure_cm< 10, 1, 0))

detailed_df <- detailed_df %>%
  mutate(in_10_to_20 = ifelse(length_of_closure_cm>= 10 & length_of_closure_cm< 20, 1, 0))

detailed_df <- detailed_df %>%
  mutate(in_20_to_30 = ifelse(length_of_closure_cm>= 20 & length_of_closure_cm< 30, 1, 0))

detailed_df <- detailed_df %>%
  mutate(in_30_to_40 = ifelse(length_of_closure_cm>= 30 & length_of_closure_cm< 40, 1, 0))

detailed_df <- detailed_df %>%
  mutate(in_40_to_50 = ifelse(length_of_closure_cm>= 40 & length_of_closure_cm< 50, 1, 0))

detailed_df <- detailed_df %>%
  mutate(in_50_to_60 = ifelse(length_of_closure_cm>= 50 & length_of_closure_cm< 60, 1, 0))

detailed_df <- detailed_df %>%
  mutate(in_60_to_70 = ifelse(length_of_closure_cm>= 60 & length_of_closure_cm< 70, 1, 0))

# Mutate flap columns to binary numeric
detailed_df <- detailed_df %>%
  mutate(
    paraspinous_flap = ifelse(paraspinous_flap == "Checked", 1, 0),
    trapezius_flap = ifelse(trapezius_flap == "Checked", 1, 0),
    latissimus_flap = ifelse(latissimus_flap == "Checked", 1, 0),
    thoracolumbar_flap = ifelse(thoracolumbar_flap == "Checked", 1, 0),
    gluteal_flap = ifelse(gluteal_flap == "Checked", 1, 0),
    rotational_flap = ifelse(rotational_flap == "Checked", 1, 0),
    rectus_flap = ifelse(rectus_flap == "Checked", 1, 0),
    v_y_flap = ifelse(v_y_flap == "Checked", 1, 0),
    other_flap = ifelse(other_flap == "Checked", 1, 0),
    local_advancement = ifelse(local_advancement == "Checked", 1, 0)
  )

#Mutate complications columns to binary numeric
detailed_df <- detailed_df %>%
  mutate(
    ssi = ifelse(ssi == "Yes", 1, 0),
    necrosis = ifelse(necrosis == "Yes", 1, 0),
    dehiscence = ifelse(dehiscence == "Yes", 1, 0),
    seroma = ifelse(seroma == "Yes", 1, 0),
    hematoma = ifelse(hematoma == "Yes", 1, 0),
    csf_leak = ifelse(csf_leak == "Yes", 1, 0),
    reoperation = ifelse(reoperation == "Yes", 1, 0),
    mortality = ifelse(mortality == "Yes", 1, 0)
  )

# Mutate risk factors columns to binary numeric
detailed_df <- detailed_df %>%
  mutate(
    htn = ifelse(htn == "Yes", 1, 0),
    hx_cva = ifelse(hx_cva == "Yes", 1, 0),
    hx_mi = ifelse(hx_mi == "Yes", 1, 0),
    aca_use = ifelse(aca_use == "Yes", 1, 0),
    neoplasm = ifelse(neoplasm == "Yes", 1, 0),
    hx_chemo = ifelse(hx_chemo == "Yes", 1, 0),
    hx_radiation = ifelse(hx_radiation == "Yes", 1, 0),
    hx_prior_spine = ifelse(hx_prior_spine == "Yes", 1, 0),
    sex = ifelse(sex == "Male", 1, 0)
  )


# Mutate categorical risk factors (smoking, diabetes)
detailed_df <- detailed_df %>%
  mutate(
    no_dm = as.integer(dm == "No"),
    niddm = as.integer(dm == "Oral Medication Managed"),
    iddm  = as.integer(dm == "Insulin Dependent")
  )

detailed_df <- detailed_df %>%
  mutate(
    never_smoker = as.integer(smoker == "Never"),
    former_smoker = as.integer(smoker == "Former"),
    current_smoker  = as.integer(smoker == "Current")
  )

# Mutate categorical risk factors (smoking, diabetes)
detailed_df <- detailed_df %>%
  mutate(
    no_dm = as.integer(dm == "No"),
    niddm = as.integer(dm == "Oral Medication Managed"),
    iddm  = as.integer(dm == "Insulin Dependent")
  )

# Mutate chronic steroid use risk factor
detailed_df <- detailed_df %>%
  mutate(
    chronic_steroid = if_else(
      steroid_use == "Chronic (> 4 weeks)",
      1, 0
    )
  )

# Mutate ssi depth
detailed_df <- detailed_df %>%
  mutate(
    deep_ssi = as.integer(infection_depth == "Deep"),
    superficial_ssi = as.integer(infection_depth == "Superficial"),
    drain_ssi  = as.integer(infection_depth == "Drain Site")
  )

# Mutate sex
detailed_df <- detailed_df %>%
  mutate(
    female = as.integer(sex == 0),
    male = as.integer(sex == 1)
  )

# Mutate primary tumor type
detailed_df <- detailed_df %>%
  mutate(
    brea_tumor = as.integer(primary_tumor_type == "Breast Cancer"),
    chor_tumor = as.integer(primary_tumor_type == "Chordoma"),
    epen_tumor = as.integer(primary_tumor_type == "Ependymoma"),
    lung_tumor = as.integer(primary_tumor_type == "Lung Cancer"),
    rena_tumor = as.integer(primary_tumor_type == "Renal Cell Carcinoma"),
    meta_tumor = as.integer(primary_tumor_type == "Metastatic Neoplasm"),
    othe_tumor = as.integer(primary_tumor_type == "Other"),
    schw_tumor = as.integer(primary_tumor_type == "Schwannoma"),
    pros_tumor = as.integer(primary_tumor_type == "Prostate"),
    mult_tumor = as.integer(primary_tumor_type == "Multiple Myeloma"),
    spin_tumor = as.integer(primary_tumor_type == "Spinal Cord Tumor")
  )

# Merge detailed_df with hx_ct_disease using study_id as the index
full_df <- detailed_df %>%
  left_join(hx_ct_disease, by = "study_id")

# Write the full data frame locally
# write.csv(full_df, "C:/Users/westo/OneDrive/Desktop/Complex Closure/full_df.csv", row.names=FALSE)

#Write the full data frame to the shared Google Drive
write_sheet(full_df, ss = "https://docs.google.com/spreadsheets/d/1_Eir3hYTAjuN0rE8Xu8mI5MvBB6wwg4muboauUkjiVE/edit?usp=sharing", sheet = "full_df_complex")