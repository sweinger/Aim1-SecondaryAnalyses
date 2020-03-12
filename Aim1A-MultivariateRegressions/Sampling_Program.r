# Cut Sample Data to send to USC for program checking

library(lubridate)
library(dplyr)
library(purrr)
library(readr)

setwd("/opt/datascience/inbound/usc")

# read in csvs - update the path parameter to reflect location of consult csvs - maybe a subfolder
ConsultDetails_all <- 
  #list.files(path=, pattern = "ds_consult_details_2019-09_10.csv") %>% 
  #map_dfr(~read_csv(.)) %>%
  read_csv("ds_consult_details_all.csv") %>%
  filter(Product=="GENERAL MEDICAL") %>%
  select(-"X1")



#Read in merged ICD list - update path parameter
ICD_Merged <- read.csv("ICD_List_Final.csv")

#Merge on ARI flag with abx appropriate/inappropriate to diagnoses
#this file includes everything on both Jeff's and TD's lists, including some non-ARIs
ConsultDetails_icd <- left_join(ConsultDetails_all,ICD_Merged,by=c("Diagnosis_1"="diagnosis_cd"),suffix=c("consults","icd"))

#sample some cancellations
Cancellations <- 
  ConsultDetails_icd %>%
  filter(Cancelled_Consult=="Y") %>%
  sample_n(500)

#sample mostly ARIs with prescription
ARI_Prescription <- 
  ConsultDetails_icd %>%
  filter(Cancelled_Consult=="N" & !is.na(Diagnosis_1) & !is.na(Prescription_1)) %>%
  sample_n(5000)

#sample mostly ARIs with no prescription
ARI_NoDrug <- 
  ConsultDetails_icd %>%
  filter(Cancelled_Consult=="N" & !is.na(Diagnosis_1) & is.na(Prescription_1)) %>%
  sample_n(2500)

#sample none of the above - < 2000 consults
OtherConsults <- 
  ConsultDetails_icd %>% 
  filter(Cancelled_Consult=="N" & is.na(Diagnosis_1)) 
  #sample_n(min(nrow(),2000))

SampleConsults <- bind_rows(Cancellations, ARI_Prescription, ARI_NoDrug, OtherConsults)

#export - specify path
write.csv(SampleConsults, "SampleConsults.csv")
