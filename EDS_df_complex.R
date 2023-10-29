#Install and load required packages
install.packages("tidyverse")
install.packages("httpuv")
install.packages("googlesheets4")
install.packages("dplyr")
library(tidyverse)
library(httpuv)
library(googlesheets4)
library(dplyr)

#Download and read full data frame from the shared Google Drive 
full_df<-read_sheet("https://docs.google.com/spreadsheets/d/1_Eir3hYTAjuN0rE8Xu8mI5MvBB6wwg4muboauUkjiVE/edit?usp=sharing", sheet = "full_df_complex")

#Filter by ct_disease
EDS_df_complex <- full_df %>%
  filter(ct_disease == "Ehlers-Danlos syndrome")

#Write the EDS data frame locally
#write.csv(EDS_df_complex, "C:/Users/westo/OneDrive/Desktop/Complex Closure/EDS_df_complex.csv", row.names=FALSE)

#Write the EDS data frame to the shared Google Drive
write_sheet(EDS_df_complex, ss = "https://docs.google.com/spreadsheets/d/1_Eir3hYTAjuN0rE8Xu8mI5MvBB6wwg4muboauUkjiVE/edit?usp=sharing", sheet = "EDS_df_complex")
